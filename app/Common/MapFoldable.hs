{-

Copyright HappyGnome 2025 (https://github.com/HappyGnome)

DISCLAIMER: 
Please don't copy this code, I'm new to Haskell and have probably made many mistakes
of style and logic. This is a learning project for the author.

-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module MapFoldable 
(
    MapFoldable,
    mapFoldr
)
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Bifunctor

class MapFoldable f c where
    mapFoldr :: (a -> b -> (c,b)) -> b -> f a -> (f c, b)

instance MapFoldable [] c where

    mapFoldr f m0 ls = 
        let
            fld x (acc,y) = first (:acc) $ f x y
        in
            foldr fld ([],m0) ls

instance (Ord k) => MapFoldable (Map.Map k) c where

    mapFoldr f m0 mp = 
        let
            fld k x (acc, y) = first (flip (Map.insert k) acc) $ f x y
        in
            Map.foldrWithKey fld (Map.empty,m0) mp

instance (Ord c) => MapFoldable Set.Set c where

    mapFoldr f m0 st = 
        let
            fld x (acc, y) = first (`Set.insert` acc) $ f x y
        in
            Set.foldr fld (Set.empty,m0) st

-- TODO: See definition of foldl' <- use this for lists preferred
-- Also see defn of foldl'. May be able to use this approach to create a mapFoldl
