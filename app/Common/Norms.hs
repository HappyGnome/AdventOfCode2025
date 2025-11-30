{-

Copyright HappyGnome 2025 (https://github.com/HappyGnome)

DISCLAIMER: 
Please don't copy this code, I'm new to Haskell and have probably made many mistakes
of style and logic. This is a learning project for the author.

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Norms (
    Normed1
    ,Normed2
    ,NormedP
    ,norm1
    ,norm2
    ,normp
    ,norm2sq
    ,normpp
    ,normInf
    ,dist1
    ,dist2
    ,distp
    ,distInf

    ) where

--import Data.Function
--import Data.Tuple.Extra

import ArithEx

--------------------------------------
--
-- | Numbers and structures admitting l1 and l infinity norms
class (Real b, Subbable a) => Normed1 a b where
    norm1 :: a -> b 
    dist1 :: a -> a -> b
    dist1 x y = norm1 $ sub x y

    normInf :: a -> b

    distInf :: a -> a -> b
    distInf x y = normInf $ sub x y

    {-# MINIMAL norm1, normInf #-}

instance (Normed1 a b) => Normed1 [a] b where
    norm1 [] = 0
    norm1 as = sum $ map norm1 as

    normInf [] = 0
    normInf as = maximum $ map normInf as

instance (Subbable a, Real c, Normed1 a c,Normed1 b c) => Normed1 (a,b) c where
    norm1 (x,y) = norm1 x + norm1 y

    normInf (x,y) = max (normInf x) (normInf y)

-- 
instance (Integral b) => Normed1 Integer b where
    norm1 = fromInteger.abs

    normInf = fromInteger.abs

instance Normed1 Double Double where
    norm1 = abs

    normInf = abs

instance (Integral b) => Normed1 Int b where
    norm1 = fromIntegral.abs

    normInf = fromIntegral.abs
---------------------------------------------------------------------
-- | Numbers and structures admitting l2 norms 
class (Subbable a) => Normed2 a where
    norm2 :: a -> Double
    norm2 x = sqrt $ norm2sq x

    dist2 :: a -> a -> Double
    dist2 x y = norm2 $ sub x y

    norm2sq :: a -> Double

    {-# MINIMAL norm2sq #-}

instance (Normed2 a) => Normed2 [a] where
    norm2sq [] = 0
    norm2sq as = sum $ map norm2sq as


instance (Normed2 a,Normed2 b) => Normed2 (a,b) where
    norm2sq (x,y) = norm2sq x + norm2sq y


instance Normed2 Double where
    norm2 = abs

    norm2sq x = x**2

instance Normed2 Int where
    norm2 = abs.realToFrac

    norm2sq x = realToFrac x ^ (2 ::Int)

instance Normed2 Integer where
    norm2 = abs.fromInteger

    norm2sq x = realToFrac x ^ (2 :: Integer)
-----------------------------------------------------------------------
-- | Numbers and structures admitting lp norms for 1 < p < infty
class (Subbable a) => NormedP a where
    normp :: Double -> a -> Double
    normp p x 
        | p <= 0 = error "Lp \"norm\" only valid for 0 < p < infty "
        | otherwise = normpp p x ** (1.0 / p)

    distp :: Double -> a -> a -> Double
    distp p x y = normp p $ sub x y

    normpp :: Double -> a -> Double

    {-# MINIMAL normpp #-}

instance (NormedP a) => NormedP [a] where
    normpp _ [] = 0
    normpp p as = sum $ map (normpp p) as


instance (Subbable a, NormedP a,NormedP b) => NormedP (a,b) where
    normpp p (x,y) = normpp p x + normpp p y

instance NormedP Double where
    normp _ = abs

    normpp p = (** p).abs

instance NormedP Int where
    normp _ = abs.realToFrac

    normpp p = (** p).abs.realToFrac

instance NormedP Integer where
    normp _ = abs.fromInteger

    normpp p = (** p).abs.fromInteger
