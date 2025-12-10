{-

Copyright HappyGnome 2025 (https://github.com/HappyGnome)

DISCLAIMER:
Please don't copy this code, I'm new to Haskell and have probably made many mistakes
of style and logic. This is a learning project for the author.

-}

-- {-# LANGUAGE ScopedTypeVariables #-}

module PuzzleAlgorithm
  ( dijkstra,
    dijkstraMultSrc,
    floydWarshall,
    lionHunt1D,
    gcdEuclid,
    gcdEuclidAll,
    lcmEuclid,
    lcmEuclidAll,
    kruskal,
    dfSearch,
    dfsMem,
    bfsTree,
    bfsMem,
  )
where

-- import Data.List

-- import qualified Data.Tree as Tree
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.SortedList as SList
import Data.Tuple
import Data.Tuple.Extra
import Debug.Trace

-- import GHC.Base
import Shorts

----------------------------------------------------------------------------------------------
-- Dijkstra
data (Ord a) => DijState a b = DijState {vstd :: Map.Map a b, actv :: SList.SortedList (b, a)}

-- | dijkstra ng sts nd
-- ng v wt = list of neighbours of v and their weights
-- sts = [(starting vertex, initial weight)] -- Multiple sources can be defined
-- nd = check a (vertex, weight) pair and return True if algorithm should end
-- returns: map of visited vertices and their weights
dijkstraMultSrc :: (Ord a, Ord b) => (a -> b -> [(a, b)]) -> [(a, b)] -> ((a, b) -> Bool) -> Map.Map a b
dijkstraMultSrc ng sts nd =
  let addVisited x wt ds = ds {vstd = Map.insert x wt $ vstd ds}

      candidateActv x wt = SList.toSortedList $ map swap $ ng x wt

      updateActv tl x wt ds = ds {actv = SList.union tl $ candidateActv x wt}

      -- tryBestActv :: (Ord a, Ord b) => a -> b -> SList.SortedList (b,a) -> DijState a b -> DijState a b
      tryBestActv x wt tl ds
        | nd (x, wt) = addVisited x wt ds
        | isJust (vstd ds Map.!? x) = dijDo ds {actv = tl}
        | otherwise = dijDo $ updateActv tl x wt $ addVisited x wt ds

      -- dijDo :: (Ord a, Ord b) => ((a,b) -> [(a,b)]) -> ((a,b) -> Bool) -> DijState a b -> DijState a b
      dijDo ds =
        case SList.uncons $ actv ds of
          Nothing -> ds
          Just ((wt, x), tl) -> tryBestActv x wt tl ds
   in vstd $ dijDo DijState {vstd = Map.empty, actv = SList.toSortedList $ map swap sts}

-- | dijkstra ng st nd
-- ng v wt = list of neighbours of v and their weights
-- st = (starting vertex, initial weight)
-- nd = check a (vertex, weight) pair and return True if algorithm should end
-- returns: map of visited vertices and their weights
dijkstra :: (Ord a, Ord b) => (a -> b -> [(a, b)]) -> (a, b) -> ((a, b) -> Bool) -> Map.Map a b
dijkstra ng st = dijkstraMultSrc ng [st]

----------------------------------------------------------------------------------------------
-- Floyd-Warshall

floydWarshall :: (Ord a, Ord b) => (a -> [(a, b)]) -> [a] -> (b -> b -> b) -> Map.Map (a, a) b
floydWarshall ng sts bop =
  let 
        tokv x = map (\(y, z) -> ((x, y), z)) $ ng x

        edg0 = concatMap tokv sts

        map1 = foldl (flip $ uncurry Map.insert) Map.empty edg0

        pairs = (\ss -> ss ++ reverse ss) $ makeAllPairs sts

        fromMaybeLiftA2 _ x _ Nothing _ = x
        fromMaybeLiftA2 _ _ y _ Nothing = y  
        fromMaybeLiftA2 f _ _ (Just x) (Just y) = f x y  

        trySetPath k mp (x, y) =
          let 
              d0 = mp Map.!? (x, k)
              d1 = mp Map.!? (k, y)
              e0 = mp Map.!? (x, y)
              e1 = liftA2 bop d0 d1
           in 
              if fromMaybeLiftA2 (<) False True e1 e0 then
                  Map.insert (x, y) (fromJust e1) mp
              else
                  mp

        iter0 mp k = foldl (trySetPath k) mp pairs
   in 
        foldl iter0 map1 sts

----------------------------------------------------------------------------------------------
-- Lion Hunting

lionHunt1D_ :: (Eq a) => (a -> Bool) -> (a -> a -> a) -> a -> a -> Maybe a
lionHunt1D_ f bis mn mx -- (not f mn) && f mx
  | md == mn || md == mx = Just mx -- mn, mx can not be further bisected.
  | f md = lionHunt1D f bis mn md
  | otherwise = lionHunt1D f bis md mx
  where
    md = bis mx mn

-- | lionHunt1D f bis mn mx
-- Try to find x y s.t. "f y && (not f x)" and either "bis x y == x" or "bis x y == y" and returns "Just y" else "Nothing"
-- E.g. if a is an integer and "bis x y = (x + y)/2" this finds the first "z" in "[mn,mx]"  s.t. "f z"
lionHunt1D :: (Eq a) => (a -> Bool) -> (a -> a -> a) -> a -> a -> Maybe a
lionHunt1D f bis mn mx
  | f mn = Just mn
  | not $ f mx = Nothing
  | otherwise = lionHunt1D_ f bis mn mx

----------------------------------------------------------------------------------------------
-- Euclid's algorithm
gcdEuclid :: (Integral a) => a -> a -> a
gcdEuclid x 0 = x
gcdEuclid x y =
  gcdEuclid y (x `mod` y)

gcdEuclidAll :: (Integral a) => [a] -> a
gcdEuclidAll [] = 1
gcdEuclidAll (x : xs) = foldl gcdEuclid x xs

-- Lowest common multiple
lcmEuclid :: (Integral a) => a -> a -> a
lcmEuclid x 0 = x
lcmEuclid 0 x = x
lcmEuclid x y = (x * y) `div` gcdEuclid x y

lcmEuclidAll :: (Integral a) => [a] -> a
lcmEuclidAll = foldl lcmEuclid 1

----------------------------------------------------------------------------------------------
-- Kruskal's algorithm
kruskal :: (Ord a, Ord b) => (a -> [(a, b)]) -> [a] -> ([(a, a, b)], [a])
kruskal gen seed =
  let
      edgeInit = sortOn thd3 $ concatMap (\a -> map (\(x, y) -> (a, x, y)) $ gen a) seed

      root anc a = maybe a (root anc) $ anc Map.!? a

      updateAnc anc a rt =
         let 
            anc' = Map.insert a rt anc
         in
            maybe anc' (\x -> updateAnc anc' x rt) $ anc Map.!? a

      -- step :: [(a,a,b)] -> Map.Map a a -> (a,a,b) -> ([(a,a,b)],Map.Map a a)
      tryEdge keeps anc totry =
        let 
            (fm, to, _) = totry
            rfm = root anc fm
            rto = root anc to

            anc' = updateAnc anc to rfm -- "fm" always in "seed", and initial set of roots is "seed", so (inductively) "rfm" (in fact all roots)  always in "seed"
         in 
            if rfm == rto
            then
                (keeps, anc)
            else
                (totry : keeps, anc')

      (resedg, resanc) = foldl (uncurry tryEdge) ([], Map.empty) edgeInit

      resrts = filter (isNothing . (Map.!?) resanc) seed -- All nodes without an ancestor are the heads of trees
   in 
      (resedg, resrts)


-- | Given a set of weighted edges (not-necessarily direcet away from their root)
--  and roots of trees, build a map of child nodes
--  Useful for putting some structure back into the output of the minimal kruskal implementation above
-- WIP TODO
--normalizeTree :: (Ord a) => [(a, a, b)] -> [a] -> Map.Map a [(a,b)]
--normalizeTree edgs rts =
--    let
--        fwdMp = toMapCons $ map (\(x,y,z) -> (x,(y,z))) edgs
--        bkMp = toMapCons $ map (\(x,y,z) -> (y,(x,z))) edgs
--
--        addX (mp,mem) x = (mp', mem')
--            where
--                nbrs m = filter (\(y,_) -> not $ Set.member y mem) $ fromMaybe [] $ m Map.!? x
--                ys = (nbrs fwdMp) union (nbrs bkMp)
--                mp' = Map.insert x ys mp
--                mem' = Set.insert x mem
--    in
        

-----------------------------------------------------------------------------------------------
-- BFS

-- | Breadth-first search to find the first level containing solutions
-- bfsTree gen pass seed
--      gen - create child states at a given parent state and level.
--              Each child node should be generated for only one parent, or duplicate solutions may result (search space assumed to be a tree)
--      pass - a state is the solution to be returned when this predicate passes
--      seed - list of states to start search from
-- returns (solns,lvl)
--      solns - list of solution nodes
--      lvl - last level searched (first level with solutions, if there are any)
bfsTree :: (a -> Int -> [a]) -> (a -> Bool) -> [a] -> ([a], Int)
bfsTree gen pass seed =
  let -- step :: [a] -> Int -> ([a],Int)
      step xs lvl =
        let passs = filter pass xs
            xs' = concatMap (`gen` lvl) xs
         in case passs of
              [] -> case xs' of
                [] -> ([], lvl)
                _ -> step xs' (lvl + 1)
              _ -> (passs, lvl)
   in step seed 0

-- | Breadth-first search to find the first level containing solutions, with memory from previous nodes visited.
--      E.g. use memory to handle search spaces that are not naturally trees
-- bfsMem gen pass seed
--      gen - create child states at a given parent state, level, and mem from previous nodes searched.
--                  Each child node should be generated for only one parent.
--      pass - a state is the solution to be returned when this predicate passes
--      seed - list of states to start search from
--      mem0 - initial memory state
-- returns (solns,lvl,mem)
--      solns - list of solution nodes
--      lvl - last level searched (first level with solutions, if there are any)
--      mem - memory state after searching all nodes in the last level
bfsMem :: (a -> Int -> b -> ([a], b)) -> (a -> b -> Bool) -> [a] -> b -> ([a], Int, b)
bfsMem gen pass seed mem0 =
  let addchilds lvl (ys, m, ps) x =
        let (zs, m') = gen x lvl m
            ps' =
              if pass x m
                then
                  x : ps
                else
                  ps
         in (zs : ys, m', ps')

      -- step :: [a] -> Int -> b -> ([a],Int,b)
      step xs lvl mem =
        let (xss', mem', passs) = foldl (addchilds lvl) ([], mem, []) xs
         in case passs of
              [] -> case xss' of
                [] -> ([], lvl, mem')
                _ -> step (concat $ reverse xss') (lvl + 1) mem'
              _ -> (passs, lvl, mem')
   in step seed 0 mem0

-----------------------------------------------------------------------------------------------
-- DFS

-- | Depth-first search to find a single solution
-- dfSearch gen pass seed
--      gen - create child states at a given parent state and level
--      pass - a state is the solution to be returned when this predicate passes
--      seed - list of states to start search from
dfSearch :: (a -> Int -> [a]) -> (a -> Bool) -> [a] -> Maybe (a, Int)
dfSearch gen pass seed =
  let -- step :: [[a]] -> Int -> Maybe (a,Int)
      step [] _ = Nothing
      step ([] : stk) lvl =  step stk (lvl - 1)
      step ((x : xs) : stk) lvl
        | pass x = Just (x, lvl)
        | otherwise = step stk' (lvl + 1)
        where
          stk' = gen x lvl : xs : stk
   in step [seed] 0

-----------------------------------------------------------------------------------------------
-- DFS with memory

-- | Depth-first search to find a single solution with memory
-- dfSearch gen pass seed mem0
--      gen - create child states and new memory state at a given parent state, level and memory state
--      pass - a state memory pair is the solution to be returned when this predicate passes
--      seed - list of states to start search from
--      mem0 - initial memory state
-- returns (soln,mem) where soln is Just the first solution found or Nothing, and mem is the final
--          memory state (excluding the update from searching the solution vertex)
dfsMem :: (a -> Int -> b -> ([a], b)) -> (a -> b -> Bool) -> [a] -> b -> (Maybe (a, Int), b)
dfsMem gen pass seed mem0 =
  let -- step :: [[a]] -> Int ->  b -> (Maybe (a,Int),b)
      step [] _ mem = (Nothing, mem)
      step ([] : stk) lvl mem = step stk (lvl - 1) mem
      step ((x : xs) : stk) lvl mem
        | pass x mem = (Just (x, lvl), mem)
        | otherwise = step stk' (lvl + 1) mem'
        where
          (chlds, mem') = gen x lvl mem
          stk' = chlds : xs : stk
   in step [seed] 0 mem0
