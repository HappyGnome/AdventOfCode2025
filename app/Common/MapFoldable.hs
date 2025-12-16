{-

Copyright HappyGnome 2025 (https://github.com/HappyGnome)

DISCLAIMER: 
Please don't copy this code, I'm new to Haskell and have probably made many mistakes
of style and logic. This is a learning project for the author.

-}

{-# LANGUAGE MultiParamTypeClasses #-} -- TODO: go back to MPTC way of handling Sets
{-# LANGUAGE FlexibleInstances #-}
 -- {-# LANGUAGE FunctionalDependencies #-}

module MapFoldable 
(
    MapFoldable,
    FacPlumbable,
    toFacFuncR,
    toFacFuncR',
    toFacFuncL,
    toFacFuncL',
    (.|),
    (|.),
    ($|),
    (|$),
    mapFoldr
)
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Bifunctor
import Data.Tuple

type FacFuncR a b c = (b -> a -> (c, a)) 
type FacFuncR' a b c = (b -> a -> (a,c)) 

type FacFuncL a b c = (a -> b -> (a, c)) 
type FacFuncL' a b c = (a -> b -> (c, a)) 

-----------------------------------------------------------

class FacPlumbable f a b c where

    toFacFuncR :: f -> FacFuncR a b c
    toFacFuncR f x y = swap $ toFacFuncL f y x

    toFacFuncL :: f -> FacFuncL a b c
    toFacFuncL f x y = swap $ toFacFuncR f y x


    toFacFuncL' :: f -> FacFuncL' a b c
    toFacFuncL' f x y = toFacFuncR f y x
    
    toFacFuncR' :: f -> FacFuncR' a b c
    toFacFuncR' f x y = swap $ toFacFuncR f x y

    (.|) :: f -> FacFuncR a d b -> FacFuncR a d c
    (.|) h g x y = uncurry (toFacFuncR h) $ g x y

    infixr 9 .|

    (|.) :: FacFuncL a c d -> f -> FacFuncL a b d
    (|.) g h x y = uncurry g $ toFacFuncL h x y 

    infixr 9 |.

    ($|) :: f -> (b,a) -> (c,a)
    ($|) = uncurry . toFacFuncR

    infixr 0 $|

    (|$) :: f -> (a,b) -> (a,c)
    (|$) = uncurry . toFacFuncL

    infixr 0 |$

    {-# MINIMAL toFacFuncR | toFacFuncL #-}

instance FacPlumbable (FacFuncL a b c) a b c where
    toFacFuncL = id

instance FacPlumbable (FacFuncR a b c) a b c where
    toFacFuncR = id

instance FacPlumbable (FacFuncL' a b c) a b c where
    toFacFuncL f x = swap . f x

instance FacPlumbable (FacFuncR' a b c) a b c where
    toFacFuncR f x = swap . f x

instance FacPlumbable (b->c) a b c where
    toFacFuncL f x y = (x ,f y)
    toFacFuncR f x y = (f x, y)
-----------------------------------------------------------

class MapFoldable f c where
    mapFoldr :: (FacPlumbable g a b c) => g -> FacFuncL' a (f b) (f c)

instance MapFoldable [] c where

    mapFoldr f' m0 ls = 
        let
            f = toFacFuncR f'
            fld x (acc,y) = first (:acc) $ f x y
        in
            foldr fld ([], m0) ls

instance (Ord k) => MapFoldable (Map.Map k) c where

    mapFoldr f' m0 mp = 
        let
            f = toFacFuncR f'
            fld k x (acc, y) = first (flip (Map.insert k) acc) $ f x y
        in
            Map.foldrWithKey fld (Map.empty,m0) mp


instance (Ord c) => MapFoldable Set.Set c where

    mapFoldr f' m0 st = 
        let
            f = toFacFuncR f'
            fld x (acc, y) = first (`Set.insert` acc) $ f x y
        in
            Set.foldr fld (Set.empty,m0) st



-- TODO: See definition of foldl' <- use this for lists preferred
-- Also see defn of foldl'. May be able to use this approach to create a mapFoldl
--
--
--

