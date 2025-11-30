{-

Copyright HappyGnome 2025 (https://github.com/HappyGnome)

DISCLAIMER: 
Please don't copy this code, I'm new to Haskell and have probably made many mistakes
of style and logic. This is a learning project for the author.

-}

module GridStep  where

import Data.Function
import MiniLinLib as MLL

data StepDir8 = Up | Dn | Lt | Rt | Ul | Ur | Dl | Dr 
    deriving (Show, Eq)

stepInDir8 :: (Num a) => StepDir8 -> V2 a -> V2 a
stepInDir8 d (V2 x y)
    | d == Up = V2 (x-1) y
    | d == Dn = V2 (x+1) y
    | d == Lt = V2 x (y-1)
    | d == Ul = V2 (x-1) (y-1)
    | d == Dl = V2 (x+1) (y-1)
    | d == Ur = V2 (x-1) (y+1)
    | d == Dr = V2 (x+1) (y+1)
    | otherwise = V2 x (y+1) --Rt

rotDir90Cw:: StepDir8 -> StepDir8
rotDir90Cw d
    | d == Up = Rt
    | d == Dn = Lt
    | d == Lt = Up
    | d == Ul = Ur
    | d == Dl = Ul
    | d == Ur = Dr
    | d == Dr = Dl 
    | otherwise = Dn --Rt

rotDir180:: StepDir8 -> StepDir8
rotDir180 d
    | d == Up = Dn
    | d == Dn = Up
    | d == Lt = Rt
    | d == Ul = Dr
    | d == Dl = Ur
    | d == Ur = Dl
    | d == Dr = Ul 
    | otherwise = Lt --Rt


instance Ord StepDir8 where
    compare =
        let
            f:: StepDir8 -> Int
            f dir
                | dir == Up = 1
                | dir == Dn = 2
                | dir == Lt = 3
                | dir == Ul = 4
                | dir == Dl = 5
                | dir == Ur = 6
                | dir == Dr = 7
                | otherwise = 8
        in
            compare `on` f
