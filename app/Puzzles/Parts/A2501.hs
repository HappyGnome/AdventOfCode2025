{-

Copyright HappyGnome 2025 (https://github.com/HappyGnome)

DISCLAIMER: 
Please don't copy this code, I'm relatively new to Haskell and have probably made many mistakes
of style and logic. This is a learning project for the author, and has been prepared for a timed challenge.

-}

{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module A2501 where

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

-- Entry point. Control which steps to run here
exec :: IO()
exec = do
    let 
        inpPathBase ="app/Puzzles/Input/A2501/"
        inpPath0 = inpPathBase ++ "Test1.txt"
        inpPath1 = inpPathBase ++ "Input1.txt"

    exec1 inpPathBase inpPath0 inpPath1
    exec2 inpPathBase inpPath0 inpPath1
    exec3 inpPathBase inpPath0 inpPath1
    execDebug inpPathBase inpPath0 inpPath1

--------------------------
-- Entry point for Part 1
exec1 :: String -> String -> String -> IO()
exec1 inpPathBase inpPath0 inpPath1 = do
    let 
        --inpPath2 = inpPathBase ++ "Test2.txt"
{-@@-} inpPath = inpPath1         -- Choose Test or Input here 

    ls <- IOH.getFileLines inpPath
    printSoln "Part 1" inpPath $ solve1 $ parseLines ls

--------------------------
-- Entry point for Part 2
exec2 :: String -> String -> String -> IO()
exec2 inpPathBase inpPath0 inpPath1 = do
    let 
        --inpPath2 = inpPathBase ++ "Test2.txt"
 {-@@-} inpPath = inpPath1         -- Choose Test or Input here 

    ls <- IOH.getFileLines inpPath
    printSoln "Part 2" inpPath $ solve2 $ parseLines ls

--------------------------
-- Entry point for Part 3
exec3 :: String -> String -> String -> IO()
exec3 inpPathBase inpPath0 inpPath1 = do
    let 
        --inpPath2 = inpPathBase ++ "Test2.txt"
 {-@@-} inpPath = inpPath0         -- Choose Test or Input here 

    ls <- IOH.getFileLines inpPath
    printSoln "Part 3" inpPath $ solve3 $ parseLines ls

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

parseLines :: [String] -> [(Int,Int)]
parseLines ls = 
    let
        fromCh cs 
            | cs == "L" = -1
            | otherwise = 1

        pline cs = 
            (fromCh [head cs],IOH.readInt $ tail cs)

    in
        map pline ls

--------------------------------------------------------------------------------------------
-- Solver

solve1 :: [(Int,Int)] ->  Maybe Int
solve1 plr =
    let 
        jmps = map (uncurry (*)) plr

        mv (at,tot) x = 
            let 
                newAt = ( at + x) `mod` 100
            in
                if newAt == 0 then
                    (newAt, tot+1)
                else
                    (newAt,tot)
    in
        Just $ snd $ foldl mv (50,0) jmps

solve2 :: [(Int,Int)] -> Maybe Int
solve2 plr = 
    let 
        jmps = map (uncurry (*)) plr

        passes 0 x =  (abs x) `div` 100
        passes at x 
            | x + at > 99 = 1 + passes (at + (min x 100) -100) (x - (min x 100))
            | x + at < 0 = 1 + passes 0 (x + at)
            | x + at < 0 =  passes (at + (max x $ -100) + 100) (x + (min (-x) $ 100))
            | x /=0 && x + at == 0 = 1
            | otherwise = 0

        mv (at,tot) x = 
            let 
                newAt = ( at + x) `mod` 100
                a = 1
                b = -100
                --ps = trace (show (a,b,passes a b)) $ passes at x
                ps =  passes at x
            in
                (newAt, tot + ps)
    in
        Just $ snd $ foldl mv (50,0) jmps

solve3 :: [(Int,Int)] -> Maybe Int
solve3 plr =
    Nothing

solveDebug :: [(Int,Int)] ->  IO()
solveDebug plr = do
    return ()
--------------------------------------------------------------------------------------------
-- Business


