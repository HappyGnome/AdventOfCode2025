{-

Copyright HappyGnome 2025 (https://github.com/HappyGnome)

DISCLAIMER: 
Please don't copy this code, I'm relatively new to Haskell and have probably made many mistakes
of style and logic. This is a learning project for the author, and has been prepared for a timed challenge.

-}

{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module A2507 where

import Text.Read
import Data.Maybe
import Data.Tuple
import Data.Tuple.Extra
import Debug.Trace
import qualified Data.Map as Map
import Text.Regex.TDFA
import Data.List
import Data.List.Extra
import Data.Ord

import IoHelpers as IOH
import MiniLinLib
import GridStep
import PuzzleAlgorithm 
import Shorts
import Norms
import ArithEx


problemNumber :: String
problemNumber = "A2507"

reps ::Int 
reps = 1000

-- Entry point. Control which steps to run here
exec :: IO()
exec = do
    let 
        inpPathBase ="app/Puzzles/Input/" ++ problemNumber ++ "/"
        inpPath0 = inpPathBase ++ "Test1.txt"
        inpPath1 = inpPathBase ++ "Input1.txt"

    exec1 inpPathBase inpPath0 inpPath1
    exec2 inpPathBase inpPath0 inpPath1
    execDebug inpPathBase inpPath0 inpPath1

--------------------------
-- Entry point for Part 1
exec1 :: String -> String -> String -> IO()
exec1 inpPathBase inpPath0 inpPath1 = do
    let 
        --inpPath2 = inpPathBase ++ "Test2.txt"
{-@@-}inpPath = inpPath1         -- Choose Test or Input here 

    readParseSolveReps (problemNumber ++ " / Part 1 orig x" ++ show reps) inpPath parseLines reps solve1'
    readParseSolveReps (problemNumber ++ " / Part 1 x"++ show reps) inpPath parseLines reps solve1
    readParseSolveReps (problemNumber ++ " / Part 1 Sorted List x"++ show reps) inpPath parseLines reps solve1SL
    readParseSolveReps (problemNumber ++ " / Part 1 Tidied Map 1 x"++ show reps) inpPath parseLines reps solve1TM1
    readParseSolveReps (problemNumber ++ " / Part 1 Tidied Map 2 x"++ show reps) inpPath parseLines reps solve1TM2

--------------------------
-- Entry point for Part 2
exec2 :: String -> String -> String -> IO()
exec2 inpPathBase inpPath0 inpPath1 = do
    let 
        --inpPath2 = inpPathBase ++ "Test2.txt"
 {-@@-}inpPath = inpPath1         -- Choose Test or Input here 

    readParseSolveReps (problemNumber ++ " / Part 2 orig x"++ show reps) inpPath parseLines reps solve2'
    readParseSolveReps (problemNumber ++ " / Part 2 x"++ show reps) inpPath parseLines reps solve2
    readParseSolveReps (problemNumber ++ " / Part 2 TidiedMaps x"++ show reps) inpPath parseLines reps solve2TM
    readParseSolveReps (problemNumber ++ " / Part 2 Sorted Lists x"++ show reps) inpPath parseLines reps solve2SL
    readParseSolveReps (problemNumber ++ " / Part 2 Pure Recursion x"++ show reps) inpPath parseLines reps solve2PR
    -- 24292631346665

--------------------------
-- Entry point for optional debug ops
execDebug :: String -> String -> String -> IO()
execDebug inpPathBase inpPath0 inpPath1 = do
    let 
        --inpPath2 = inpPathBase ++ "Test2.txt"
        inpPath = inpPath0         -- Choose Test or Input here 

    ls <- IOH.getFileLines inpPath
    solveDebug $ parseLines ls

--------------------------------------------------------------------------------------------
--IO

type ParseLineResult = Int --- Best to replace ParseLineResult with the actual type, if it's simple enough.
                            -- Defining this here just to allow us to warm up the compilet on the blank file

parseLines :: [String] -> (Map.Map (V2 Int) Char,Int)
parseLines ls = 
    let
        onChar c
            | c == '.' = Nothing
            | otherwise = Just c
    in
        (loadJust2D onChar ls, length ls)

--------------------------------------------------------------------------------------------
-- Solver

solve1 :: (Map.Map (V2 Int) Char, Int) ->  Maybe Int --
solve1 (plr, rows) = -- @@ 1562
    let 
        mp' = Map.filter (=='^') plr
        startAt = fst $ head $ Map.toList $ Map.filter(=='S') plr

        f wk n = ( wk'' , n + length hitSplt)
            where 
                
                wk' = map (stepInDir8 Dn) wk 
                hitSplt = filter (\v -> isJust $ mp' Map.!? v) wk'
                noHitSplt = filter (\v -> isNothing $ mp' Map.!? v) wk'

                aftrSplit = union (map (stepInDir8 Lt) hitSplt) (map (stepInDir8 Rt) hitSplt)

                wk'' = noHitSplt `union` aftrSplit

        g = (!! rows) $ iterate (uncurry f) ([startAt],0) 

    in
        Just $ snd g
 
solve2 :: (Map.Map (V2 Int) Char, Int) -> Maybe Integer
solve2 (plr,rows) = -- @@
    let 
        mp' = Map.filter (=='^') plr
        startAt = fst $ head $ Map.toList $ Map.filter(=='S') plr

        f wk = wk''
            where 
                wk' = map (first (stepInDir8 Dn)) wk 

                hitSplt = filter (\p -> isJust $ mp' Map.!? fst p) wk'
                noHitSplt = filter (\p -> isNothing $ mp' Map.!? fst p) wk'

                aftrSplitL = map (first (stepInDir8 Lt)) hitSplt 

                aftrSplitR = map (first (stepInDir8 Rt)) hitSplt

                wk'' = map (second (sumOn' snd)) $ groupOnKey fst $ sortOn fst $ noHitSplt ++ aftrSplitL ++ aftrSplitR

        g = (!! rows) $ iterate f [(startAt,1)] 
    in 
        Just $ sumOn' snd g

---------------------------------------------------------------------------
-- Rough versions to compare

solve1' :: (Map.Map (V2 Int) Char, Int) ->  Maybe Int --
solve1' (plr, rows) = -- @@
    let 
     -- TODO Map keys function?
        mp' = Map.filter (=='^') plr
        startAt = fst $ head $ Map.toList $ Map.filter(=='S') plr

        f wk n = ( wk'' , n + length hitSplt)
            where 
                wk' = Map.fromList $ map (\(k,v) -> (stepInDir8 Dn k,v)) $ Map.toList wk 
                hitSplt = Map.filterWithKey (\k _ -> isJust $ mp' Map.!? k) wk'
                noHitSplt = Map.filterWithKey (\k _ -> isNothing $ mp' Map.!? k) wk'

                aftrSplit = Map.fromList $ 
                        union (map (\(k,v) -> (stepInDir8 Lt k,v)) $ Map.toList hitSplt) 
                                (map (\(k,v) -> (stepInDir8 Rt k,v)) $ Map.toList hitSplt)

                wk'' = noHitSplt `Map.union` aftrSplit

        g = foldl (\acc _ -> (uncurry f) acc) (Map.fromList [(startAt,True)],0) [0..rows] 

    in
        Just $ snd g

solve2' :: (Map.Map (V2 Int) Char, Int) -> Maybe Integer
solve2' (plr,rows) = -- @@
    let 
        mp' = Map.filter (=='^') plr
        startAt = fst $ head $ Map.toList $ Map.filter(=='S') plr

        f wk = wk''
            where 
                wk' = Map.fromList $ map (\(k,v) -> (stepInDir8 Dn k,v)) $ Map.toList wk 

                hitSplt = Map.filterWithKey (\k _ -> isJust $ mp' Map.!? k) wk'
                noHitSplt = Map.filterWithKey (\k _ -> isNothing $ mp' Map.!? k) wk'

                aftrSplitL = Map.fromList $ 
                                (map (\(k,v) -> (stepInDir8 Lt k, v)) $ Map.toList hitSplt) 

                aftrSplitR = Map.fromList $ 
                                (map (\(k,v) -> (stepInDir8 Rt k,v)) $ Map.toList hitSplt)

                wk'' = Map.unionWith (+) (Map.unionWith (+) noHitSplt aftrSplitL) aftrSplitR

        g = foldl (\acc _ -> f acc) (Map.fromList [(startAt,1)]) [0 .. rows] 
    in 
        Just $ sum g
---------------------------------------------------------------------------
-- "Optimized"?

solve1TM1 :: (Map.Map (V2 Int) Char, Int) ->  Maybe Int --
solve1TM1 (plr, rows) = -- @@
    let 
        mp' = Map.filter (=='^') plr
        startAt = fst $ head $ Map.toList $ Map.filter(=='S') plr

        f wk n = ( wk'' , n + length hitSplt)
            where 
                wk' = Map.fromList $ map (first (stepInDir8 Dn)) $ Map.toList wk 
                hitSplt = Map.filterWithKey (\k _ -> isJust $ mp' Map.!? k) wk'
                noHitSplt = Map.filterWithKey (\k _ -> isNothing $ mp' Map.!? k) wk'

                aftrSplit = Map.fromList $ 
                        union (map (first (stepInDir8 Lt)) $ Map.toList hitSplt) 
                                (map (first (stepInDir8 Rt)) $ Map.toList hitSplt)

                wk'' = noHitSplt `Map.union` aftrSplit

        g = (!! rows ) $ iterate (uncurry f) (Map.fromList [(startAt,True)],0) 
    in
        Just $ snd g

solve1TM2 :: (Map.Map (V2 Int) Char, Int) ->  Maybe Int --
solve1TM2 (plr, rows) = -- @@
    let 
        mp' = Map.filter (=='^') plr
        startAt = fst $ head $ Map.toList $ Map.filter(=='S') plr

        f wk n = ( wk'' , n + length hitSplt)
            where 
                wk' = mapKey (stepInDir8 Dn) wk 
                hitSplt = Map.filterWithKey (\k _ -> isJust $ mp' Map.!? k) wk'
                noHitSplt = Map.filterWithKey (\k _ -> isNothing $ mp' Map.!? k) wk'

                aftrSplit = Map.union (mapKey (stepInDir8 Lt) hitSplt) 
                                (mapKey (stepInDir8 Rt) hitSplt)

                wk'' = noHitSplt `Map.union` aftrSplit

        g = (!! rows ) $ iterate (uncurry f) (Map.fromList [(startAt,True)],0) 

    in
        Just $ snd g

solve1SL :: (Map.Map (V2 Int) Char, Int) ->  Maybe Int --
solve1SL (plr, rows) = -- @@
    let 
        mp' = Map.filter (=='^') plr
        startAt = fst $ head $ Map.toList $ Map.filter(=='S') plr

        f wk n = ( wk'' , n + length hitSplt)
            where 
                wk' = map (stepInDir8 Dn) wk 

                hitSplt = filter (\k -> isJust $ mp' Map.!? k) wk'
                noHitSplt = filter (\k -> isNothing $ mp' Map.!? k) wk'

                aftrSplit = unionSorted
                                (map (stepInDir8 Lt) hitSplt) 
                                (map (stepInDir8 Rt) hitSplt)

                wk'' = noHitSplt `unionSorted` aftrSplit

        g = (!! rows ) $ iterate (uncurry f) ([startAt],0) 

    in
        Just $ snd g

-- Tidied version of the rough algorithm (with Maps)
solve2TM :: (Map.Map (V2 Int) Char, Int) -> Maybe Integer
solve2TM (plr,rows) = -- @@
    let 
        mp' = Map.filter (=='^') plr
        startAt = fst $ head $ Map.toList $ Map.filter(=='S') plr

        f wk = wk''
            where 
                wk' =mapKey (stepInDir8 Dn) wk 

                hitSplt = Map.filterWithKey (\k _ -> isJust $ mp' Map.!? k) wk'
                noHitSplt = Map.filterWithKey (\k _ -> isNothing $ mp' Map.!? k) wk'

                aftrSplitL = mapKey (stepInDir8 Lt) hitSplt 

                aftrSplitR = mapKey (stepInDir8 Rt) hitSplt

                wk'' = Map.unionWith (+) (Map.unionWith (+) noHitSplt aftrSplitL) aftrSplitR

        g = (!! rows) $ iterate f (Map.fromList [(startAt,1)]) 
    in 
        Just $ sum g

-- Sorted list approach
solve2SL :: (Map.Map (V2 Int) Char, Int) -> Maybe Integer
solve2SL (plr,rows) = -- @@
    let 
        mp' = Map.filter (=='^') plr

        startAt = fst $ head $ Map.toList $ Map.filter(=='S') plr

        f wk = wk''
            where 
                wk' = map (first (stepInDir8 Dn)) wk 

                hitSplt = filter (\p -> isJust $ mp' Map.!? fst p) wk'

                noHitSplt = filter (\p -> isNothing $ mp' Map.!? fst p) wk'

                aftrSplitL = map (first (stepInDir8 Lt)) hitSplt 

                aftrSplitR = map (first (stepInDir8 Rt)) hitSplt

                unionOp = unionOfSortedBy fst (\(x,y) (_,z) -> (x, y + z))

                wk'' = unionOp (unionOp  aftrSplitR noHitSplt) aftrSplitL

        g = (!! rows) $ iterate f [(startAt,1)] 
    in 
        Just $ sumOn' snd g

 -- One-shot recursion as used in this solution https://github.com/jonathanpaulson/AdventOfCode/blob/master/2025/7.py
 -- Looks as slow as the concat list version
solve2PR :: (Map.Map (V2 Int) Char, Int) -> Maybe Integer
solve2PR (plr,rows) = -- @@
    let 
        mp' = Map.filter (=='^') plr
        startAt = fst $ head $ Map.toList $ Map.filter(=='S') plr

        f n x mem
            | n > rows = (1, Map.fromList [(x,1)])
            | Just fv <- mem Map.!? x = (fv,mem)
            | isJust $ mp' Map.!? y0 = (f12, Map.insert x f12 mem2)
            | otherwise = (f0, Map.insert x f0 mem0) 
            where 
                y0 = stepInDir8 Dn x
                y1 = stepInDir8 Dl x
                y2 = stepInDir8 Dr x
                (f0,mem0) = f m y0 mem
                (f1,mem1) = f m y1 mem
                (f2,mem2) = f m y2 mem1
                f12 = f1 + f2
                m = n + 1

    in 
        Just $ fst $ f 1 startAt Map.empty

-- | Naive recursion - looks good, but scales very badly 
solve2PR' :: (Map.Map (V2 Int) Char, Int) -> Maybe Integer
solve2PR' (plr,rows) = -- @@
    let 
        mp' = Map.filter (=='^') plr
        startAt = fst $ head $ Map.toList $ Map.filter(=='S') plr

        f n x
            | n > rows = 1
            | isJust $ mp' Map.!? y0 = f m y1 + f m y2
            | otherwise = f m y0 
            where 
                y0 = stepInDir8 Dn x
                y1 = stepInDir8 Dl x
                y2 = stepInDir8 Dr x
                m = n + 1

    in 
        Just $ f 1 startAt
---------------------------------------------------------------------------

solveDebug :: (Map.Map (V2 Int) Char, Int) ->  IO()
solveDebug plr = do
    return ()
--------------------------------------------------------------------------------------------
-- Business

mapKey :: (Ord b) => (a -> b) -> Map.Map a c -> Map.Map b c
mapKey ff = Map.foldlWithKey (\acc k v -> Map.insert (ff k) v acc ) Map.empty

unionOfSortedBy' :: (Ord b) => (a -> b) -> (a -> a -> a) -> [a] -> [a] -> [a] -> [a]
unionOfSortedBy' _ _ [] ks acc = reverse acc ++ ks 
unionOfSortedBy' _ _ ls [] acc = reverse acc ++ ls 
unionOfSortedBy' fby fof (l:ls) (k:ks) acc
    | fbyl > fbyk = unionOfSortedBy' fby fof (l:ls) ks (k : acc)
    | fbyl < fbyk = unionOfSortedBy' fby fof ls (k:ks) (l : acc)
    | otherwise = unionOfSortedBy' fby fof ls ks (fof k l : acc)
    where
        fbyl = fby l
        fbyk = fby k

unionOfSortedBy ::(Ord b) => (a -> b) -> (a -> a -> a) -> [a] -> [a] -> [a]
unionOfSortedBy fby fof ls ks = unionOfSortedBy' fby fof ls ks []

unionSorted' :: (Ord a) => [a] -> [a] -> [a] -> [a]
unionSorted' [] ks acc = reverse acc ++ ks 
unionSorted' ls [] acc = reverse acc ++ ls 
unionSorted' (l:ls) (k:ks) acc
    | l > k = unionSorted' (l:ls) ks (k : acc)
    | k > l = unionSorted' ls (k:ks) (l : acc)
    | otherwise = unionSorted' ls ks (k : acc)
  
unionSorted :: (Ord a) => [a] -> [a] -> [a]
unionSorted ls ks = unionSorted' ls ks []
