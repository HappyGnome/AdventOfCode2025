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

    readParseSolve (problemNumber ++ " / Part 1") inpPath parseLines solve1

--------------------------
-- Entry point for Part 2
exec2 :: String -> String -> String -> IO()
exec2 inpPathBase inpPath0 inpPath1 = do
    let 
        --inpPath2 = inpPathBase ++ "Test2.txt"
 {-@@-}inpPath = inpPath1         -- Choose Test or Input here 

    readParseSolve (problemNumber ++ " / Part 2") inpPath parseLines solve2
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

        g = foldl (\acc _ -> uncurry f acc) ([startAt],0) [0..rows] 

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

        g = foldl (\acc _ -> f acc) ([(startAt,1)]) [0 .. rows] 
    in 
        Just $ sumOn' snd g

solveDebug :: (Map.Map (V2 Int) Char, Int) ->  IO()
solveDebug plr = do
    return ()
--------------------------------------------------------------------------------------------
-- Business


