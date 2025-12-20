

{-

Copyright HappyGnome 2025 (https://github.com/HappyGnome)

DISCLAIMER: 
Please don't copy this code, I'm new to Haskell and have probably made many mistakes
of style and logic. This is a learning project for the author.

-}


-- {-# LANGUAGE DefaultSignatures #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE DeriveGeneric #-}

module SparseTable (
         SparseTable
         , frromList
         , insert
         , update
         , alter
         ) where

import Data.Function
import ArithEx -- Type claesses for basic arithmetic properties
import Norms
import qualified Data.Map as Map
import qualified Data.Set as Set

------------------------------------------------------------------------------------------
--
data (Ord j, Ord k) => SparseTable j k a = SparseTable {stRows :: Map.Map j (Set.Set k), stCols :: Map.Map k (Set.Set j), stValues :: Map.Map (j,k) a } deriving (Show, Eq)

fromList :: (Ord j, Ord k) => [(j,k,a)] -> SparseTable j k a
fromList ls =
    let
        values = Map.fromList $ map (\(x,y,z) -> ((x,y),z)) ls
        keysC = map Set.fromList $ toMapCons $ map swap $ Map.keys values
        keysR = map Set.fromList $ toMapCons $ Map.keys values

    in
        SparseTable cols rows values


insert :: (Ord j, Ord k) => j -> k -> a -> SparseTable j k a -> SparseTable j k a
insert j k a (SparseTable rows cols values) = 
    let
        values' = Map.insert (j,k) a values 
        rows' = Map.alter (maybe (Just $ Set.fromList [k]) (Just . Set.insert k)) j rows
        cols' = Map.alter (maybe (Just $ Set.fromList [j]) (Just . Set.insert j)) k cols 
    in
        SparseTable rows' cols' values'

update :: (Ord j, Ord k) => (a -> Maybe a) -> j -> k -> SparseTable j k a -> SparseTable j k a
update f j k (SparseTable rows cols values)
    let
        exists0 = Map.member (j,k) values'

        values' = Map.update f (j,k) values 

        exists = Map.member (j,k) values'

        rows'
            | exists0 && not exists = Map.update (Set.remove k) j rows
            | otherwise = rows

        cols'
            | exists0 && not exists = Map.update (Set.remove j) k cols
            | otherwise = cols
    in
        SparseTable rows' cols' values'

alter :: (Ord j, Ord k) => (Maybe a -> Maybe a) -> j -> k -> SparseTable j k a -> SparseTable j k a
alter f j k (SparseTable rows cols values) = 
    let
        exists0 = Map.member (j,k) values'

        values' = Map.alter f (j,k) values 

        exists = Map.member (j,k) values'

        rows'
            | exists0 && not exists = Map.update (Set.remove k) j rows
            | exists && not exists0 = Map.update (Set.insert k) j rows
            | otherwise = rows

        cols'
            | exists0 && not exists = Map.update (Set.remove j) k cols
            | exists && not exists0 = Map.update (Set.insert j) k rows
            | otherwise = cols
    in
        SparseTable rows' cols' values'







