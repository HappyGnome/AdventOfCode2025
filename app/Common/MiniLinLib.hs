
{-

Copyright HappyGnome 2025 (https://github.com/HappyGnome)

DISCLAIMER: 
Please don't copy this code, I'm new to Haskell and have probably made many mistakes
of style and logic. This is a learning project for the author.

-}


-- {-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module MiniLinLib (
         fromTuple2
         ,asTuple2
         ,fromTuple3
         ,asTuple3
         ,V2(V2)
         ,v2x
         ,v2y
         ,V3(V3)
         ,v3x
         ,v3y
         ,v3z
         , vLin
         ,ivMod
         ,Vector
         ,Zero
         ,zero) where

import Data.Function
import ArithEx -- Type claesses for basic arithmetic properties
import Norms

import Control.DeepSeq
import GHC.Generics

----------------------------
-- General linear
class Zero c where
    zero :: c

class (Scalable b, Summable (b a), Subbable (b a), Zero (b a)) =>  Vector b a 

vLin :: (Num a, Vector b a) => [(b a,a)] -> b a
vLin = 
    foldl (\acc (v, c) -> acc `add` (c >* v)) zero  

----------------------------
-- 2D
--
-- Classes
class FromTuple2 c where
    fromTuple2 :: (a,a) -> c a

class AsTuple2 c where
    asTuple2 :: c a -> (a,a) 

-- Data
data M2x2 a = M2x2 a a a a
    deriving (Show, Eq)

data V2 a = V2 {v2x :: a, v2y :: a}
    deriving (Show, Eq, Generic)

-- Instances
instance (Num a, Ord a) => Ord (V2 a) where
    compare = compare `on` asTuple2

instance Functor V2 where
    fmap f (V2 x y) = V2 (f x) (f y)
    (<$) a _ = V2 a a

instance FromTuple2 V2 where
    fromTuple2 (x,y) = V2 x y

instance AsTuple2 V2 where
    asTuple2 (V2 x y) = (x,y)

instance Scalable V2 where
    (>*) x (V2 y z) = V2 (x Prelude.* y) (x Prelude.* z)

instance (Num a) => Summable (V2 a) where
    add (V2 x y) (V2 a b) = 
        V2 (x Prelude.+ a) (y Prelude.+ b)

instance (Num a) => Subbable (V2 a) where
    sub (V2 x y) (V2 a b) = 
        V2 (x Prelude.- a) (y Prelude.- b)

instance (Num a) => Zero (V2 a) where
    zero = V2 0 0

instance (NFData a) => NFData (V2 a)

instance (Normed2 a, Num a) => Normed2 (V2 a) where
    norm2sq (V2 x y) = norm2sq x + norm2sq y 

instance (Subbable a, Real c, Normed1 a c, Num a) => Normed1 (V2 a) c where
    norm1 (V2 x y) = norm1 x + norm1 y 

    normInf (V2 x y) = max (normInf x) (normInf y)
--
-- 2D methods




---------------------
--Integral 

ivMod :: (Integral a) => V2 a -> V2 a -> V2 a
ivMod (V2 x y) (V2 x' y') = 
    V2 (x `mod` x') (y `mod` y')

----------------------------
-- 3D
--
-- Classes
class FromTuple3 c where
    fromTuple3 :: (a,a,a) -> c a

class AsTuple3 c where
    asTuple3 :: c a -> (a,a,a) 

-- Data
data V3 a = V3 {v3x :: a, v3y :: a, v3z :: a}
    deriving (Show, Eq, Generic)

-- Instances
instance (Num a, Ord a) => Ord (V3 a) where
    compare = compare `on` asTuple3

instance Functor V3 where
    fmap f (V3 x y z) = V3 (f x) (f y) (f z)
    (<$) a _ = V3 a a a

instance FromTuple3 V3 where
    fromTuple3 (x,y,z) = V3 x y z

instance AsTuple3 V3 where
    asTuple3 (V3 x y z) = (x,y,z)

instance Scalable V3 where
    (>*) x (V3 y z a) = V3 (x Prelude.* y) (x Prelude.* z) (x Prelude.* a)

instance (Num a) => Summable (V3 a) where
    add (V3 x y z) (V3 a b c) = 
        V3 (x Prelude.+ a) (y Prelude.+ b) (z Prelude.+ c)

instance (Num a) => Subbable (V3 a) where
    sub (V3 x y z) (V3 a b c) = 
        V3 (x Prelude.- a) (y Prelude.- b) (z Prelude.- c)

instance (Num a) => Zero (V3 a) where
    zero = V3 0 0 0

instance (NFData a) => NFData (V3 a)

instance (Normed2 a, Num a) => Normed2 (V3 a) where
    norm2sq (V3 x y z) = norm2sq x + norm2sq y + norm2sq z

instance (Subbable a, Real c, Normed1 a c, Num a) => Normed1 (V3 a) c where
    norm1 (V3 x y z) = norm1 x + norm1 y + norm1 z

    normInf (V3 x y z) = maximum [normInf x, normInf y, normInf z]
--
-- 3D methods





