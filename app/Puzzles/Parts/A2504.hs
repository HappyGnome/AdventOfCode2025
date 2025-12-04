{-

Copyright HappyGnome 2025 (https://github.com/HappyGnome)

DISCLAIMER: 
Please don't copy this code, I'm relatively new to Haskell and have probably made many mistakes
of style and logic. This is a learning project for the author, and has been prepared for a timed challenge.

-}

{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module A2504 where

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
problemNumber = "A2504"

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

-- | Read input, parse, call solver and print output, plus basic timing
readParseSolve :: (PuzzleSolution b) => String -> String -> ([String] -> a) -> (a -> Maybe b) -> IO()
readParseSolve name inpPath parse solve = do

    (ls, readTime) <- IOH.runTimedIO IOH.getFileLines inpPath

    let
        psd =parse ls

        soln = solve psd

    (_,parseSolveTime) <- IOH.runTimedIO (printSoln ( problemNumber ++ "/" ++ name) inpPath) soln 

    putStrLn $ "Read time: " ++ show readTime ++ "  Parse & Solve: " ++ show parseSolveTime


--------------------------
-- Entry point for Part 1
exec1 :: String -> String -> String -> IO()
exec1 inpPathBase inpPath0 inpPath1 = do
    let 
        --inpPath2 = inpPathBase ++ "Test2.txt"
{-@@-}inpPath = inpPath1         -- Choose Test or Input here 

    readParseSolve "Part 1" inpPath parseLines solve1

--------------------------
-- Entry point for Part 2
exec2 :: String -> String -> String -> IO()
exec2 inpPathBase inpPath0 inpPath1 = do
    let 
        --inpPath2 = inpPathBase ++ "Test2.txt"
 {-@@-}inpPath = inpPath1         -- Choose Test or Input here 

    readParseSolve "Part 2" inpPath parseLines solve2

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

parseLines :: [String] -> Map.Map (V2 Int) Bool
parseLines = 
    IOH.loadJust2D (\x -> if x == '@' then Just True else Nothing)

--------------------------------------------------------------------------------------------
-- Solver

solve1 :: Map.Map (V2 Int) Bool ->  Maybe Int
solve1 plr = -- @@
    let
        allAdj = [Up,Dn,Lt,Rt,Ul,Ur,Dl,Dr]
        nbrs p = filter id $ mapMaybe (\dir -> plr Map.!? stepInDir8 dir p) allAdj
        validCell p _ = length (nbrs p) < 4
    in
      Just $ length $ Map.toList $ Map.filter id $ Map.mapWithKey validCell plr


solve2 :: Map.Map (V2 Int) Bool -> Maybe Int
solve2 plr = -- @@
    let
        allAdj = [Up,Dn,Lt,Rt,Ul,Ur,Dl,Dr]
        nbrs plr' p = filter id $ mapMaybe (\dir -> plr' Map.!? stepInDir8 dir p) allAdj
        validCell plr' p _ = length (nbrs plr' p) < 4

        nextMap cnt plr' 
            | new > 0 = nextMap (cnt + new) $  Map.map not $ Map.filter not candidate 
            | otherwise = cnt
            where
                candidate = Map.mapWithKey (validCell plr') plr'
                new = length $ Map.filter id candidate

    in
      Just $ nextMap 0 plr


solveDebug :: Map.Map (V2 Int) Bool ->  IO()
solveDebug plr = do
    let
        allAdj = [Up,Dn,Lt,Rt,Ul,Ur,Dl,Dr]
        nbrs plr' p = filter id $ mapMaybe (\dir -> plr' Map.!? stepInDir8 dir p) allAdj
        validCell plr' p _ = length (nbrs plr' p) < 4

        nextMap cnt plr' 
            | new > 0 = (print () >> rende candidate)  >> (rende $ Map.filter not candidate) >> (nextMap (cnt + new) $ Map.map not $ Map.filter not candidate )
            | otherwise = print cnt
            where
                candidate = Map.mapWithKey (validCell plr') plr'
                new = length $ Map.filter id candidate

        rend (Just True) = '@'
        rend (Just False) = '!'
        rend _ = '.'

        rende = IOH.renderGrid 10 10 rend
        
    nextMap 0 plr
--------------------------------------------------------------------------------------------
-- Business


