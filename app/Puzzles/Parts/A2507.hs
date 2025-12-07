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

    readParseSolve' (problemNumber ++ " / Part 1") inpPath parseLines solve1

--------------------------
-- Entry point for Part 2
exec2 :: String -> String -> String -> IO()
exec2 inpPathBase inpPath0 inpPath1 = do
    let 
        --inpPath2 = inpPathBase ++ "Test2.txt"
 {-@@-}inpPath = inpPath1         -- Choose Test or Input here 

    readParseSolve' (problemNumber ++ " / Part 2") inpPath parseLines solve2

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
solve1 (plr, rows) = -- @@
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

--        mp' = Map.filter (=='^') plr
--        allSplitrs = length mp'
--
--        hasNbr v _ = (==) (Just '^') $ mp' Map.!? (stepInDir8 Rt $ stepInDir8 Rt v)
--        ign = length $ Map.filterWithKey hasNbr mp'
--    in 
--        Just $ traceShow (allSplitrs, ign) $ (allSplitrs) - 1

solve2 :: (Map.Map (V2 Int) Char, Int) -> Maybe Integer
solve2 (plr,rows) = -- @@
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
        Just $ traceShow (length mp') $ sum g

solveDebug :: (Map.Map (V2 Int) Char, Int) ->  IO()
solveDebug plr = do
    return ()
--------------------------------------------------------------------------------------------
-- Business


