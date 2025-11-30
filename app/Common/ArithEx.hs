{-

Copyright HappyGnome 2025 (https://github.com/HappyGnome)

DISCLAIMER: 
Please don't copy this code, I'm new to Haskell and have probably made many mistakes
of style and logic. This is a learning project for the author.

-}


module ArithEx (
        Subbable
        ,Summable
        ,Scalable
        ,sub
        ,add
        ,(>*)
        ,(*<)
    ) where

-- import Data.Ratio

--------------------------------------
--Subbable
--
class Subbable c where
    sub :: c -> c -> c

infixr 5 `sub`

instance Subbable Int where
    sub = (-)

instance Subbable Integer where
    sub = (-)

instance Subbable Double where
    sub = (-)

instance Subbable Float where
    sub = (-)

instance (Subbable a) => Subbable [a] where
    sub = zipWith sub

instance (Subbable a, Subbable b) => Subbable (a,b) where
    sub (x0,y0) (x1,y1) = (x0 `sub` x1, y0 `sub` y1)

--------------------------------------
-- Summable
class Summable c where
    add :: c -> c -> c 

infixr 5 `add`

instance Summable Int where
    add = (+)

instance Summable Integer where
    add = (+)

instance Summable Double where
    add = (+)

instance Summable Float where
    add = (+)

instance (Summable a) => Summable [a] where
    add = zipWith add

instance (Summable a, Summable b) => Summable (a,b) where
    add (x0,y0) (x1,y1) = (x0 `add` x1, y0 `add` y1)

--------------------------------------
-- Scalable
--
class Scalable c where
    (>*) :: (Num a) => a -> c a -> c a
    (>*) = flip (*<)
    
    (*<) :: (Num a) => c a -> a -> c a
    (*<) = flip (>*)

    {-# MINIMAL (>*) | (*<) #-}

infixr 5 >*
infixl 5 *<

-- instance (Scalable c a) => Scalable [c] a where
--     (>*) x = map (x >*)
-- 
-- instance (Scalable c a, Scalable d a) => Scalable (c,d) a where
--     (>*) x (y,z) = (x >* y, x >* z)
