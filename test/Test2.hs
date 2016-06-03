module Test2
    (
    f, f0, f1, f3,
    DataType(Constr1), D1(..), D2
    , AClass(..), BClass(..), Class(..)
    , New(..)
    , Foo, Foo2, Bar
     ) where

---------------
-- FUNCTIONS --
---------------
-- unchanged function
f :: a -> a -> a
f = undefined

-- changed type, but compatible
f0 :: b -> b
f0 = undefined

-- changed type, incompatible
f1 :: String -> String
f1 = undefined

-- not exported --> equal to deleted function
f2 :: Num a => a -> a
f2 = undefined

-- new function
f3 :: Int -> Int
f3 b = undefined

----------------
-- DATA TYPES --
----------------

-- removed deriving to test instances
data DataType
    = Constr1 Int String
    | Constr2 [Int]
    | Constr3 (Int,Int)

-- Changed type var of dat constructor
data D1 b = Con b

-- Changed arity
data D2 a b = Empty

--------------
-- NEWTYPES --
--------------
--Unchanged
newtype New = New Int

-------------------
-- TYPE SYNONYMS --
-------------------
--Added type synonym Bar as extra indirection
type Foo = Bar

-- Changed type from Int to the incompatible type Bar
type Foo2  = Bar

type Bar = DataType


------------------
-- TYPE CLASSES --
------------------
-- Unchanged type class
class AClass a where
    aMethod :: a -> a

-- New type class
class BClass a where
    test :: a

-- Added method with default implementation
class Class a where
    m :: a -> Int

    m2 :: a -> Int
    m2 = m

---------------
-- INSTANCES --
---------------
-- Unchanged
instance Class DataType where
    m = undefined

-- Added this instance
instance Class Int where
    m = undefined

-- deleted from Datatype, but added manual
instance Show DataType where
    show = undefined
