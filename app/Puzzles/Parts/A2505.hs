{-

Copyright HappyGnome 2025 (https://github.com/HappyGnome)

DISCLAIMER: 
Please don't copy this code, I'm relatively new to Haskell and have probably made many mistakes
of style and logic. This is a learning project for the author, and has been prepared for a timed challenge.

-}

{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module A2505 where

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


problemNumber :: String --Hi
problemNumber = "A2505"

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

parseLines :: [String] -> ([(Int,Int)],[Int])
parseLines ls = 
    let
        blocks = IOH.splitLinesOnEmpty ls
        ings = map readInt $ blocks !! 1
        rngs = map (\es -> (head es, es !! 1)) ends
            where
                ends = map (map readInt . IOH.splitOnPred (=='-') ) $ head blocks
    in
        (rngs,ings)

--------------------------------------------------------------------------------------------
-- Solver

solve1 :: ([(Int,Int)],[Int]) ->  Maybe Int
solve1 (rngs,ings) = -- @@
    let
        inRng ing (x,y) = ing >= x && ing <= y 
        good ing = any (inRng ing) rngs
    in
        Just $ length $ filter good ings

solve2 :: ([(Int,Int)],[Int]) -> Maybe Int
solve2 (rngs,ings) = -- @@
    let
        isect (a,b) (c,d) = d>=a && c<=b

        unionRng (a,b) (c,d) = (min a c, max b d)
        
        srtd = sort rngs

        f fin x [] = x:fin 
        f fin x (y:ys)
            | isect x y =  f fin (unionRng x y) ys
            | otherwise = f (x:fin) y ys
            
        g =  f [] (head srtd) (tail srtd)

    in
        Just $  sum $ map (\(x,y) -> 1 + y - x) g


--solve2 :: ([(Int,Int)],[Int]) -> Maybe Int
--solve2 (rngs,ings) = -- @@
--    let
--        f acc (x,y) = foldl (\mp z -> Map.insert z True mp) acc [x..y]
--        g = foldl f Map.empty rngs
--
--    in
--        Just $ length $ Map.toList g
--
solveDebug :: ([(Int,Int)],[Int]) ->  IO()
solveDebug (rngs,ings) = do
    return ()
--------------------------------------------------------------------------------------------
-- Business


