{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Semver where

import           Typechecker

import           Control.Monad (when, forM, forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.List (partition)

import           GHC (Ghc, Target, ModuleName, ModuleGraph,
                      ModSummary, TypecheckedModule, PackageKey, Name, TyThing
                      )
import qualified GHC
import           GHC.Paths (libdir)
import qualified Var as GHC

import           Digraph (flattenSCCs)
import           HscTypes (isBootSummary)
import           Id (idName)
import           IdInfo (IdDetails(..))
import           InstEnv (ClsInst(..))
import           Name (getOccString)
import           Outputable (Outputable, showSDoc, ppr)
import           TcUnify (tcSubType)
import           TcType (UserTypeCtxt(FunSigCtxt,ConArgCtxt))
import           TyCon (isDataTyCon, tyConDataCons_maybe, tyConName, tyConKind, tcExpandTyCon_maybe)
import           DataCon (dataConOrigResTy,dataConRepType)
import           Type (eqType)
import           TcRnTypes (TcGblEnv(..))
import           Class (classOpItems, ClassOpItem(..), Class(classTyCon), DefMeth(..))
import           Control.Arrow ((***))

----------------
-- DATA TYPES --
----------------

-- Represents changes between different Haskell Modules
data Change = Major String | Minor String | Patch String

-- Represents the module interface. Contains all information needed to
-- check the compatibility of a module.
data ModuleInterface = ModuleInterface
    { tcMod          :: TypecheckedModule
    , modName        :: ModuleName
    , modExportName  :: [Name]
    , modMethods     :: [TyThing]
    , modFunctions   :: [TyThing]
    , modDataTypes   :: [TyThing]
    , modTypeClasses :: [TyThing]
    , modNewTypes    :: [TyThing]
    , modInstances   :: [ClsInst]
    , modTySyns      :: [TyThing]
    , modTyThings    :: [TyThing]
    }

data MyClassOpItem = COI GHC.Id DefMeth

-- | The module target; either a source module that needs to be compiled,
-- or the name of a module that is installed or has a local source.
data ModuleGoal
    = SourceGoal GHC.TargetId
        -- ^ a GHC compilation target
    | ModuleGoal ModuleName (Maybe PackageKey)
        -- ^ name and optional `PackageKey`; when @Nothing@ the module might
        -- be a compilation target
    deriving (Eq)


---------------
-- INSTANCES --
---------------

instance Eq Change where
  (Major _) == (Major _) = True
  (Minor _) == (Minor _) = True
  (Patch _) == (Patch _) = True
  _         == _         = False

instance Ord Change where
  (Patch _) < (Minor _) = True
  (Patch _) < (Major _) = True
  (Minor _) < (Major _) = True
  _         < _         = False
  x         <= y         = (x == y) || (x < y)

instance GHC.NamedThing MyClassOpItem where
    getName (COI id _) = GHC.getName id

---------------
-- FUNCTIONS --
---------------
-- Main function of this library.
-- Input is a boolean, which toggles the verbosity and two filepaths.
-- Output is written to the console.
diffModule :: Bool -> FilePath -> FilePath -> IO ()
diffModule  verbose old new = do
    newIF <- loadModule new
    withModule old $ \oldIF -> do
          dflags <- GHC.getSessionDynFlags

          change1 <- checkFunctions oldIF newIF
          change2 <- checkTySyns oldIF newIF
          change3 <- checkDataTypes oldIF newIF
          change4 <- checkNewTypes oldIF newIF
          let change5 = checkTypeClasses oldIF newIF
              change6 = checkInstances oldIF newIF
              res = change1 ++ change2 ++ change3 ++ change4 ++ change5 ++ change6
              finalChange = if null res
                             then [Patch "No incompatible interface changes"]
                             else res
          if verbose
            then liftIO $ putStrLn $ "All changes in this module:\n"++ printChanges finalChange ++"\n"
            else liftIO $ putStr ""
          liftIO $ putStrLn "RESULT:"
          let maxChange = maximum finalChange
              output = case maxChange of
                        Major _ -> "MAJOR CHANGE"
                        Minor _ -> "MINOR CHANGE"
                        Patch _ -> "PATCH CHANGE"
          liftIO $ putStrLn output

-------------------------
-- LOADING OF A MODULE --
-------------------------

loadModule :: FilePath -> IO ModuleInterface
loadModule file = withModule file return

withModule :: FilePath -> (ModuleInterface -> Ghc a) -> IO a
withModule file cont = withGhc $ guessGoal file >>= makeInterface >>= cont

-- | Run a simple GHC session with default session flags
withGhc :: Ghc a -> IO a
withGhc ghcAction =
    GHC.runGhc (Just libdir) $ do
        -- prepare package state, ignoring output (no need for linking)
        dynFlags <- GHC.getSessionDynFlags
        _ <- GHC.setSessionDynFlags dynFlags
        GHC.defaultCleanupHandler dynFlags ghcAction


-- | Guess the intended `ModuleGoal` from a `String`. When given a path to a source
-- file, it will be compiled to produce the interface. When given the name of
-- a module, the corresponding local source file will be used if available;
-- otherwise, the module will be searched for in the package database.
guessGoal :: String -> Ghc ModuleGoal
guessGoal s = do
    GHC.Target tid _ _ <- GHC.guessTarget s Nothing
    return $ case tid of
        GHC.TargetModule n -> ModuleGoal n Nothing
        _                  -> SourceGoal tid

-- | Produce a `ModuleInterface` for each compilation target in
-- topological order.
moduleInterfacesForTargets ::
    [Target] ->    -- filepaths and/or module names
    Ghc [ModuleInterface]
moduleInterfacesForTargets targets = do
    GHC.setTargets targets

    moduleGraph <- GHC.depanal [] False

    when (GHC.needsTemplateHaskell moduleGraph) $
        error "readModuleInterface: Template Haskell unsupported"

    forM (listModSummaries moduleGraph) $ \modSummary ->
        GHC.parseModule modSummary >>=
        GHC.typecheckModule >>=
        makeModuleInterface
  where
    -- Sort the modules topologically and discard hs-boot modules.
    -- The topological order is not currently used, but it will be
    -- necessary in the future
    listModSummaries :: ModuleGraph -> [ModSummary]
    listModSummaries = filter (not . isBootSummary) . sortGraph

    sortGraph :: ModuleGraph -> ModuleGraph
    sortGraph g = flattenSCCs $ GHC.topSortModuleGraph False g Nothing


makeInterface :: ModuleGoal -> Ghc ModuleInterface
makeInterface g = case g of
    SourceGoal tid -> do
        let obj_allowed = True
        last <$>  -- last here means that the module we loaded is the
                  -- last in the dependency chain
            moduleInterfacesForTargets [GHC.Target tid obj_allowed Nothing]
    _ -> error "ModuleGoal not implemented yet"
    -- ModuleGoal modName mPkgKey -> do
    --     packageModuleInterface modName mPkgKey

-- Extract all data from a typechecked module and insert it into a moduleInterface.
makeModuleInterface :: TypecheckedModule -> Ghc ModuleInterface
makeModuleInterface tcMod = do
    _ <- GHC.loadModule tcMod
    let ghcModule = GHC.ms_mod . GHC.pm_mod_summary $ GHC.tm_parsed_module tcMod
        modInfo   = GHC.moduleInfo tcMod
        tyThings  = GHC.modInfoTyThings modInfo
        exportedTyThings = filter  (GHC.modInfoIsExportedName modInfo . GHC.getName) tyThings
        (methods,   rest)  = partition isMethod exportedTyThings
        (functions, rest1) = partition isFunction rest
        (datatypes, rest2) = partition isDataType rest1
        (tycls, rest3) = partition isTypeClass rest2
        (newtypes, rest4) = partition isNewType rest3
        (tysyns, rest') = partition isTypeSyn rest4
        insts = GHC.modInfoInstances modInfo

    return $ ModuleInterface
              tcMod
              (GHC.moduleName ghcModule)
              (GHC.modInfoExports modInfo)
              methods
              functions
              datatypes
              tycls
              newtypes
              insts
              tysyns
              rest'

----------------------
-- SUBTYPE CHECKING --
----------------------

checkSubType :: TypecheckedModule -> GHC.Name -> GHC.Type -> GHC.Type -> Ghc Bool
checkSubType tcMod name t1 t2 = do
      equal <- eqType' t1 t2
      let theSubtypeCheck = tcSubType (FunSigCtxt name) t2 t1
          (tcGblEnv, _) = GHC.tm_internals_ tcMod
      session <- GHC.getSession
      (_msgs, mbResult) <- liftIO $ initTcFromModGuts session tcGblEnv theSubtypeCheck
      let res = case mbResult of
                  Nothing -> False
                  Just _  -> True
      return $ equal || res

eqType':: GHC.Type -> GHC.Type -> Ghc Bool
eqType' t1 t2 = do
    dflags <- GHC.getSessionDynFlags
    let pprT1 = showSDoc dflags $ ppr t1
        pprT2 = showSDoc dflags $ ppr t2
    return $ pprT1 == pprT2

--------------------------------
-- COMPATIBILITY OF FUNCTIONS --
--------------------------------
checkFunctions :: ModuleInterface -> ModuleInterface -> Ghc [Change]
checkFunctions old new =
    let oldFun = map extractId $ modFunctions old
        newFun = map extractId $ modFunctions new
        combinedFun = combineNamedThings oldFun newFun
    in checkFunction (tcMod new) combinedFun


checkFunction :: TypecheckedModule -> [(Maybe GHC.Id,Maybe GHC.Id)] -> Ghc [Change]
checkFunction _ [] = return []
checkFunction tcMod (h:rest) =
  do
    restChanges <- checkFunction tcMod rest
    case h of
      (Nothing, Just x) -> return $ Minor ("Added Function " ++ printName x) : restChanges
      (Just x, Nothing) -> return $ Major ("Removed Function " ++ printName x) : restChanges
      (Just x, Just y) -> do
                            changes <- isChangedFunction tcMod x y
                            return $ changes ++ restChanges
      _ -> error "checkFunction: impossible case"

isChangedFunction :: TypecheckedModule -> GHC.Id -> GHC.Id -> Ghc [Change]
isChangedFunction tcMod old new = do
    let oldTy = GHC.varType old
        newTy = GHC.varType new
    bool1 <- eqType' oldTy newTy
    bool2 <- checkSubType tcMod (GHC.getName old) oldTy newTy
    if not bool1 && bool2
        then return [Minor ("Type of function " ++ printName old ++ " has changed, but remains compatible")]
        else return [Major ("Type of Function " ++ printName old ++ " is not compatible anymore") | not bool1 && not bool2 ]

 ---------------------------------
 -- COMPATIBILITY OF DATA TYPES --
 ---------------------------------

checkDataTypes :: ModuleInterface -> ModuleInterface -> Ghc [Change]
checkDataTypes old new =
    let oldData =  map extractTyCon $ modDataTypes old
        newData = map extractTyCon $ modDataTypes new
        combinedData = combineNamedThings oldData newData
    in checkDataType (tcMod new) combinedData

checkDataType :: TypecheckedModule -> [(Maybe GHC.TyCon,Maybe GHC.TyCon)] -> Ghc [Change]
checkDataType _ [] = return []
checkDataType tcmod (h:rest) = do
    restChanges <- checkDataType tcmod rest
    case h of
      (Nothing, Just x) -> return $ Minor ( "Added datatype " ++ printName x) : restChanges
      (Just x, Nothing) -> return $ Major ("Removed datatype " ++ printName x) : restChanges
      (Just x, Just y) -> do
                            changed <- isChangedDataType tcmod x y
                            return $ changed ++ restChanges
      _ -> error "checkDataType: impossible case"

isChangedDataType :: TypecheckedModule -> GHC.TyCon -> GHC.TyCon -> Ghc [Change]
isChangedDataType tcmod old new = do
   change1 <- checkDataCons tcmod (GHC.tyConDataCons old) (GHC.tyConDataCons new)
   let bool = GHC.tyConArity old /= GHC.tyConArity new
       change2 = [Major $ "changed arity of datatype " ++ printName old | bool ]
       change = change1 ++ change2
   return change

--------------------------------
-- COMPATIBILITY OF DATA CONS --
--------------------------------

checkDataCons :: TypecheckedModule -> [GHC.DataCon] -> [GHC.DataCon] -> Ghc [Change]
checkDataCons tcmod old new = do
    let combinedDataCons = combineNamedThings old new
    checkDataCon tcmod combinedDataCons

checkDataCon :: TypecheckedModule -> [(Maybe GHC.DataCon, Maybe GHC.DataCon)] -> Ghc [Change]
checkDataCon _ [] = return []
checkDataCon tcmod (h:rest) = do
    restChanges <- checkDataCon tcmod rest
    case h of
      (Nothing, Just x) -> return $ Major ("Added datacon " ++ printName x): restChanges
      (Just x, Nothing) -> return $ Major ("Removed datacon " ++ printName x): restChanges
      (Just x , Just y) -> do
                            changes <- isChangedDataCon tcmod x y
                            return $ changes ++ restChanges

isChangedDataCon :: TypecheckedModule -> GHC.DataCon -> GHC.DataCon -> Ghc [Change]
isChangedDataCon tcmod old new = do
    let oldTy = dataConRepType old
        newTy = dataConRepType new
    bool1 <- checkSubType tcmod (GHC.getName old) oldTy newTy
    bool2 <- checkSubType tcmod (GHC.getName old) newTy oldTy
    if bool1 && bool2
      then return [Minor ("datcon " ++ printName old ++ " has changed, but remains compatible")]
      else return [Major ("Incompatible type change of datacon " ++ printName old)]

-----------------------------------
-- COMPATIBILITY OF TYPE CLASSES --
-----------------------------------

checkTypeClasses :: ModuleInterface -> ModuleInterface -> [Change]
checkTypeClasses old new =
    let oldTyCl = map extractTyClass $ modTypeClasses old
        newTyCl = map extractTyClass $ modTypeClasses new
        combinedTyCl = combineNamedThings oldTyCl newTyCl
    in checkTypeClass combinedTyCl

checkTypeClass :: [(Maybe GHC.Class, Maybe GHC.Class)] -> [Change]
checkTypeClass [] = []
checkTypeClass (h:rest) =
    case h of
      (Nothing, Just x) -> Minor ("Added TypeClass " ++ printName x) : checkTypeClass rest
      (Just x, Nothing) -> Major ("Removed TypeClass " ++ printName x) : checkTypeClass rest
      (Just x, Just y) -> isChangedTypeClass x y ++ checkTypeClass rest

isChangedTypeClass :: GHC.Class -> GHC.Class -> [Change]
isChangedTypeClass old new =
    let oldMethods = classOpItems old
        newMethods = classOpItems new
        bool1 = GHC.tyConArity (classTyCon old) /= GHC.tyConArity (classTyCon new)
        change1 = [Major ("Changed arity of TypeClass " ++ printName old) | bool1]
        change2 = checkTyClassMethods oldMethods newMethods
    in change1 ++ change2


-----------------------------------------
-- COMPATIBILITY OF TYPE CLASS METHODS --
-----------------------------------------
coiWrapper :: ClassOpItem -> MyClassOpItem
coiWrapper = uncurry COI

coiUnWrapper :: MyClassOpItem -> ClassOpItem
coiUnWrapper (COI a b) = (a,b)

checkTyClassMethods :: [ClassOpItem] -> [ClassOpItem] -> [Change]
checkTyClassMethods old new =
    let oldCoi = map coiWrapper old
        newCoi = map coiWrapper new
        combinedMethods = combineNamedThings oldCoi newCoi
        cMethods = map (fmap coiUnWrapper Control.Arrow.*** fmap coiUnWrapper) combinedMethods
    in checkTyClassMethod cMethods

checkTyClassMethod :: [(Maybe ClassOpItem, Maybe ClassOpItem)] -> [Change]
checkTyClassMethod [] = []
checkTyClassMethod (h:rest) =
    case h of
      (Just x, Nothing) -> Major ("Removed TypeClass method " ++ (printName . fst) x) : checkTyClassMethod rest
      (Nothing, Just x) -> case snd x of
                             NoDefMeth -> Major ("Added TypeClass method " ++ (printName .fst) x ++ " without default implementation") : checkTyClassMethod rest
                             _         -> Minor ("Added TypeClass method " ++ (printName .fst) x ++ " with default implementation") : checkTyClassMethod rest
      (Just x, Just y) -> if snd x == snd y
                          then checkTyClassMethod rest
                          else case snd x of
                                 NoDefMeth -> checkTyClassMethod rest
                                 _         -> Major ("Removed default implementation of TypeClass method " ++ (printName . fst) x) : checkTyClassMethod rest


-------------------------------
-- COMPATIBILITY OF NEWTYPES --
-------------------------------
checkNewTypes :: ModuleInterface -> ModuleInterface -> Ghc [Change]
checkNewTypes old new = do
    let oldNewTypes =  map extractTyCon $ modNewTypes old
        newNewTypes = map extractTyCon $ modNewTypes new
        combinedData = combineNamedThings oldNewTypes newNewTypes
    checkNewType (tcMod new) combinedData

checkNewType :: TypecheckedModule -> [(Maybe GHC.TyCon,Maybe GHC.TyCon)] -> Ghc [Change]
checkNewType _ [] = return []
checkNewType tcmod (h:rest) = do
    restChanges <- checkNewType tcmod rest
    case h of
      (Nothing, Just x) -> return $ Minor ( "Added newtype " ++ printName x) : restChanges
      (Just x, Nothing) -> return $ Major ("Removed newtype " ++ printName x) : restChanges
      (Just x, Just y) -> do
                            changed <- isChangedNewType tcmod x y
                            return $ changed ++ restChanges
      _ -> error "checkDataType: impossible case"

isChangedNewType :: TypecheckedModule -> GHC.TyCon -> GHC.TyCon -> Ghc [Change]
isChangedNewType tcmod old new = do
    change1 <- checkDataCons tcmod (GHC.tyConDataCons old) (GHC.tyConDataCons new)
    let bool = GHC.tyConArity old /= GHC.tyConArity new
        change2 = [Major $ "changed arity of newtype " ++ printName old | bool ]
        change = change1 ++ change2
    return change

--------------------------------
-- COMPATIBILITY OF INSTANCES --
--------------------------------
checkInstances :: ModuleInterface -> ModuleInterface -> [Change]
checkInstances old new =
    let oldInstances = modInstances old
        newInstances = modInstances new
        combinedData = combineNamedThings oldInstances newInstances
    in checkInstance combinedData

checkInstance :: [(Maybe ClsInst,Maybe ClsInst)] -> [Change]
checkInstance [] = []
checkInstance (h:rest) = case h of
      (Nothing, Just x) -> Minor ( "Added instance " ++ printName x) : checkInstance rest
      (Just x, Nothing) -> Major ("Removed instance " ++ printName x) : checkInstance rest
      (Just x, Just y) -> checkInstance rest
      _ -> error "checkDataType: impossible case"


------------------------------------
-- COMPATIBILITY OF TYPE SYNONYMS --
------------------------------------
checkTySyns :: ModuleInterface -> ModuleInterface -> Ghc [Change]
checkTySyns old new = do
    let oldTySyns = map extractTyCon $ modTySyns old
        newTySyns = map extractTyCon $ modTySyns new
        combinedData = combineNamedThings oldTySyns newTySyns
    checkTySyn combinedData

checkTySyn :: [(Maybe GHC.TyCon,Maybe GHC.TyCon)] -> Ghc [Change]
checkTySyn [] = return []
checkTySyn (h:rest) = do
  restChanges <- checkTySyn rest
  case h of
    (Nothing, Just x) -> return $ Minor ( "Added type synonym " ++ printName x) : restChanges
    (Just x, Nothing) -> return $ Major ("Removed type synonym " ++ printName x) : restChanges
    (Just x, Just y) -> do
                          changes <- isChangedTySyn x y
                          return $ changes ++ restChanges
    _ -> error "checkDataType: impossible case"

isChangedTySyn :: GHC.TyCon -> GHC.TyCon -> Ghc [Change]
isChangedTySyn old new= do
    let oldType = tcExpandTyCon_maybe old []
        newType = tcExpandTyCon_maybe new []
    case oldType of
        Just (_,t1,_) -> case newType of
                          Just (_,t2,_) -> do
                                  equal <- eqType' t1 t2
                                  if equal
                                    then return []
                                    else return  [Major ("Definition of type synonym " ++ printName old ++ " has changed")]
                          Nothing -> return [Major ("Definition of type synonym " ++ printName old ++ " has been changed in an incompatible manner")]
        Nothing -> case newType of
                      Just (_,t2,_)-> return [Major ("Definition of type synonym " ++ printName old ++ " has been changed in an incompatible manner")]
                      Nothing -> return []

-----------------------
-- UTILITY FUNCTIONS --
-----------------------

combineNamedThings :: GHC.NamedThing a => [a] -> [a] -> [(Maybe a, Maybe a)]
combineNamedThings [] [] = []
combineNamedThings [] (h:rest) = (Nothing, Just h) : combineNamedThings [] rest
combineNamedThings (h:rest) [] = (Just h, Nothing) : combineNamedThings [] rest
combineNamedThings (hold:restOld) newList =
  let (hnew, restnew) = partition (\id -> printName id == printName hold) newList
    in case hnew of
        [] -> (Just hold, Nothing) : combineNamedThings restOld restnew
        [match] -> (Just hold, Just match) : combineNamedThings restOld restnew
        _ -> error $ "combineNamedThings: returned too many matches for: " ++ printName hold

isMethod :: TyThing -> Bool
isMethod (GHC.AnId (GHC.idDetails -> ClassOpId _)) = True
isMethod _ = False

isFunction :: TyThing -> Bool
isFunction (GHC.AnId (GHC.idDetails -> VanillaId)) = True
isFunction _ = False

isDataType :: TyThing -> Bool
isDataType (GHC.ATyCon tc) = isDataTyCon tc
isDataType _               = False

isTypeClass :: TyThing -> Bool
isTypeClass (GHC.ATyCon tc) = GHC.isClassTyCon tc
isTypeClass _               = False

isNewType :: TyThing -> Bool
isNewType (GHC.ATyCon tc) = GHC.isNewTyCon tc
isNewType _               = False

isTypeSyn :: TyThing -> Bool
isTypeSyn (GHC.ATyCon tc) = GHC.isTypeSynonymTyCon tc
isTypeSyn _               = False

extractId :: TyThing -> GHC.Id
extractId (GHC.AnId anId) = anId
extractId _               = error "Not an Id"

extractTyClass :: TyThing -> GHC.Class
extractTyClass (GHC.ATyCon tc) =
    case GHC.tyConClass_maybe tc of
      Just t -> t
      _      -> error "Not a TyClass"
extractTyClass _               = error "Not a TyClass"

extractTyCon :: TyThing -> GHC.TyCon
extractTyCon tyThing  = case tyThing of
    GHC.ATyCon tc -> tc
    _             -> error "Not a TyCon"

printName :: (GHC.NamedThing a) => a -> String
printName = getOccString


printChanges :: [Change] -> String
printChanges changes = unlines $ map printChange changes

printChange :: Change -> String
printChange change = case change of
   Major s -> "Major Change : " ++ s
   Minor s -> "Minor Change : " ++ s
   Patch s -> "Patch Change : " ++ s
