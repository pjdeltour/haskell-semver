module Test1
    (
    f, f0, f1, f2,
    DataType(Constr1), D1(..), D2

    , AClass(..), Class(..)
    , New(..)
    , Foo, Foo2
     ) where

---------------
-- FUNCTIONS --
---------------
f :: a -> a -> a
f = undefined

f0 :: a -> a
f0 = undefined

f1 :: Int -> Int
f1 = undefined

f2 :: Num a => a -> a
f2 = undefined

----------------
-- DATA TYPES --
----------------
data DataType
    = Constr1 Int String
    | Constr2 [Int]
    deriving (Eq, Show)

data D1 a = Con a

data D2 a = Empty

--------------
-- NEWTYPES --
--------------
newtype New = New Int

-------------------
-- TYPE SYNONYMS --
-------------------
type Foo = DataType

type Foo2 = Int

------------------
-- TYPE CLASSES --
------------------
class AClass a where
    aMethod :: a -> a

class Class a where
    m :: a -> Int

---------------
-- INSTANCES --
---------------
instance Class DataType where
    m = undefined

instance Class Bool where
    m = undefined
