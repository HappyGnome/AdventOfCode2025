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
         SparseTable()
         , fromList
         , fromList'
         , toList'
         , toList
         , fromDiag
         , fromCol
         , fromRow
         , insert
         , update
         , alter
         , union
         , unionWith
         , map
         , mapKeys
         , mapRowKeys
         , mapColKeys
         , transpose
         , alterCol
         , alterRow
         ) where

import Data.Function
import ArithEx -- Type claesses for basic arithmetic properties
import Norms
import qualified Data.Map as Map
import qualified Data.Set as Set

----------------------------------------------
-- Private

toKV :: (a,b,c) -> ((a,b),c)
toKV (x,y,z) = ((x,y),z)

fromKV :: ((a,b),c) -> (a,b,c)
fromKV ((x,y),z) = (x,y,z)
------------------------------------------------------------------------------------------
--
data (Ord j, Ord k) => SparseTable j k a = SparseTable {stRows :: Map.Map j (Set.Set k), stCols :: Map.Map k (Set.Set j), stValues :: Map.Map (j,k) a }

------------------------------------
-- Instances
--
instance (Ord j, Ord k) => Show (SparseTable j k a) where
    show st = "fromList : " ++ show ( sort $ toList st )

instance (Ord j, Ord k) => Eq (SparseTable j k a) where
    (==) st0 st1 = (stValues st0) == (stValues st1)

------------------------------------
-- Constructors

-- | Sparse table with no elements
empty :: (Ord j, Ord k) => SparseTable j k a
empty = SparseTable Map.empty Map.empty Map.empty

-- | Convert list with keys as tuples to SparseTable
fromList' :: (Ord j, Ord k) => [((j,k),a)] -> SparseTable j k a
fromList' ls =
    let
        values = Map.fromList ls
        keysC = map Set.fromList $ toMapCons $ map swap $ Map.keys values
        keysR = map Set.fromList $ toMapCons $ Map.keys values

    in
        SparseTable cols rows values

-- | Convert list with (key0,key1,value) tuples to SparseTable
fromList :: (Ord j, Ord k) => [(j,k,a)] -> SparseTable j k a
fromList ls = fromList' $ map toKV ls

fromDiag :: (Ord j, Num j, Ord k, Num k) => [a] -> SparseTable j k a
fromDiag ls = fromList' $ zip [(x,x) | x <- [0,1,..]] ls

fromCol :: (Ord j, Num j, Ord k) => k -> [a] -> SparseTable j k a
fromCol x ls = fromList' $ zip [(y,x) | y <- [0,1,..]] ls

fromRow :: (Ord j, Ord k, Num k) => k -> [a] -> SparseTable j k a
fromRow x ls = fromList' $ zip [(x,y) | y <- [0,1,..]] ls

fromArray :: [[a]]-> SparseTable Int Int a
fromArray xss = 
    let
        xss' = concat $ mapWithIndex (\ i xs -> mapWithIndex (\j x -> ((i,j),x))) xss
    in
        fromList' xss'

------------------------------------------------------
-- Converters

-- | Convert SparseTable to list with keys as tuples
toList' (Ord j, Ord k) => SparseTable j k a -> [(j,k,a)]
toList' st = Map.toList $ stValues st

-- | Convert SparseTable to list
toList (Ord j, Ord k) => SparseTable j k a -> [(j,k,a)]
toList st = map fromKV $ toList' st

---------------------------------------------------------------------
-- Basic modifications

-- | Insert or replace an element in a sparse table
insert :: (Ord j, Ord k) => j -> k -> a -> SparseTable j k a -> SparseTable j k a
insert j k a (SparseTable rows cols values) = 
    let
        values' = Map.insert (j,k) a values 
        rows' = Map.alter (maybe (Just $ Set.fromList [k]) (Just . Set.insert k)) j rows
        cols' = Map.alter (maybe (Just $ Set.fromList [j]) (Just . Set.insert j)) k cols 
    in
        SparseTable rows' cols' values'

-- | Update or delete an existing element
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

-- | Add, update or delete an element
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

-- | Left-biased union
union :: (Ord j, Ord k) => SparseTable j k a -> SparseTable j k a -> SparseTable j k a
union (SparseTable rows cols values) (SparseTable rows' cols' values') = 
    let
        values'' = Map.union values values'

        rows'' = Map.unionWith (Set.union) rows rows'

        cols'' = Map.unionWith (Set.union) cols cols'

    in
        SparseTable rows'' cols'' values''

-- | Union with an operation called for each key (row,col) in the union. 
-- The first argument is never called with Nothing Nothing
unionWith :: (Ord j, Ord k) => (Maybe a -> Maybe b -> c) -> SparseTable j k a -> SparseTable j k b -> SparseTable j k c
unionWith f (SparseTable rows cols values) (SparseTable rows' cols' values') = 
    let
        valuesR = Map.difference values' values
        valuesL = Map.foldlWithKey (\acc ky v -> Map.insert ky (f (Just v) (values' Map.!? ky)) acc) Map.empty values
        
        values'' = Map.union valuesL $ Map.map (f Nothing . Just) valuesR

        rows'' = Map.unionWith (Set.union) rows rows'

        cols'' = Map.unionWith (Set.union) cols cols'

    in
        SparseTable rows'' cols'' values''
--
-- | Union with an operation called for each key (row,col) in the first table. 
-- The first argument is never called with Nothing Nothing
unionWithL :: (Ord j, Ord k) => (a -> Maybe a -> a) -> SparseTable j k a -> SparseTable j k a -> SparseTable j k a
unionWithL f (SparseTable rows cols values) (SparseTable rows' cols' values') = 
    let
        valuesR = Map.difference values' values
        valuesL = Map.foldlWithKey (\acc ky v -> Map.insert ky (f (Just v) (values' Map.!? ky)) acc) Map.empty values
        
        values'' = Map.union valuesL $ valuesR

        rows'' = Map.unionWith (Set.union) rows rows'

        cols'' = Map.unionWith (Set.union) cols cols'

    in
        SparseTable rows'' cols'' values''



map :: (Ord j, Ord k) => (a -> b) -> SparseTable j k a -> SparseTable j k b
map f st = st {stValues = Map.map f $ stValues st}

filter :: (Ord j, Ord k) => (a->Bool) -> SparseTable j k a -> SparseTable j k b
filter f (SparseTable rows cols values) =
    let
        values' = Map.filter f values
        keysRem = Map.keys $ Map.difference values values'

        remRow r (x,y) = Map.update (Set.delete y) x r
        remCol c (x,y) = Map.update (Set.delete x) y c

        (rows',cols') = foldl (\(r,c) xy -> (remRow r xy, remCol c xy) ) (rows,cols) keysRem

    in
        SparseTable rows' cols' values'

filterWithKey :: (Ord j, Ord k) => (j -> k -> a->Bool) -> SparseTable j k a -> SparseTable j k b
filterWithKey f (SparseTable rows cols values) =
    let
        values' = Map.filterWithKey (uncurry f) values
        keysRem = Map.keys $ Map.difference values values'

        remRow r (x,y) = Map.update (Set.delete y) x r
        remCol c (x,y) = Map.update (Set.delete x) y c

        (rows',cols') = foldl (\(r,c) xy -> (remRow r xy, remCol c xy) ) (rows,cols) keysRem

    in
        SparseTable rows' cols' values'

-----------------------------------------------------------------------
-- Reshaping
--

mapKeys :: (Ord j, Ord k) => (j->j) -> (k->k) -> SparseTable j k a -> SparseTable j k a
mapKeys f g (SparseTable rows cols values) =
     let
        values' = Map.mapKeys (\(x',y') -> (f x', g y')) values

        rows' = Map.mapKeys f rows

        cols' = Map.mapKeys g cols
    in
        SparseTable rows' cols' values'

mapRowKeys :: (Ord j, Ord k) => (j->j) -> SparseTable j k a -> SparseTable j k a
mapRowKeys f (SparseTable rows cols values) =
     let
        values' = Map.mapKeys (first f) values

        rows' = Map.mapKeys f rows

    in
        SparseTable rows' cols values'

mapColKeys :: (Ord j, Ord k) => (k->k) -> SparseTable j k a -> SparseTable j k a
mapColKeys g (SparseTable rows cols values) =
     let
        values' = Map.mapKeys (second g) values

        cols' = Map.mapKeys g cols
    in
        SparseTable rows cols' values'

transpose :: (Ord j, Ord k) => SparseTable j k a -> SparseTable j k a
transpose (SparseTable rows cols values) =
     let
        values' = Map.mapKeys (\(x',y') -> (y',x')) values
    in
        SparseTable cols rows values'

---------------------------------------------------------------------
-- Row / Column ops
--

updateAt :: (Ord j, Ord k, Foldable t) => (a -> Maybe a) -> t (j,k) ->  SparseTable j k a -> SparseTable j k a
updateAt f ls st = foldl (update f) st ls

alterAt :: (Ord j, Ord k, Foldable t) => (Maybe a -> Maybe a) -> t (j,k) ->  SparseTable j k a -> SparseTable j k a
alterAt f ls st = foldl (alter f) st ls

alterCol :: (Ord j, Ord k) => (a -> a) -> k ->  SparseTable j k a -> SparseTable j k a
alterCol f y st =
    | Nothing <- cs' = st
    | Just cs <- cs' = updateAt f (map (,y) cs) st
    where
        cs' = (stCols st) Map.!? y

alterRow :: (Ord j, Eq j, Ord k) => (a -> a) -> j ->  SparseTable j k a -> SparseTable j k a
alterRow f x st =
    | Nothing <- rs' = st
    | Just rs <- rs' = updateAt f (map (x,) rs) st
    where
        rs' = (stRows st) Map.!? x

swapRows :: (Ord j, Ord k) => j -> j ->  SparseTable j k a -> SparseTable j k a
swapRows x0 x1 st =
    let 
        swap x
            | x == x0 = x1
            | x == x1 = x0
            | otherwise = x
    in
        mapRowKeys swap st -- It would be faster to implement this directly (no need to visit all elements)
    
swapCols :: (Ord j, Ord k, Eq k) => k -> k ->  SparseTable j k a -> SparseTable j k a
swapCols y0 y1 st =
    let 
        swap y
            | y == y0 = y1
            | y == y1 = y0
            | otherwise = y
    in
        mapColKeys swap st -- It would be faster to implement this directly (no need to visit all elements)

getCol :: (Ord j, Ord k) => k ->  SparseTable j k a -> SparseTable j k a
getCol y = filterWithKey (\_ y' _ -> y' == y) st
        
getRow :: (Ord j, Ord k) => j ->  SparseTable j k a -> SparseTable j k a
getRow x = filterWithKey (\x' _ _ -> x' == x) st

combineCols :: (Ord j, Ord k) => k -> k -> (a -> Maybe a -> a) -> SparseTable j k a -> SparseTable j k a
combineCols from to f st = 
    let 
        col = mapColKeys (const to) $ getCol from st
    in
        unionWithL f col st

combineRows :: (Ord j, Ord k) => j -> j -> (a -> Maybe a -> a) -> SparseTable j k a -> SparseTable j k a
combineRows from to f st = 
    let 
        row = mapRowKeys (const to) $ getRow from st
    in
        unionWithL f row st

--------------------------------------------------------------------------------------
-- Matrix ops

-- Mult
--
{-
mult :: (Ord j, Ord k) => (a -> a -> a) -> SparseTable j k a -> SparseTable j k a
mult 
--WIP
-}


-- LUP decomp
--

-- Solve
--



