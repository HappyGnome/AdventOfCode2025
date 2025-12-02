{-

Copyright HappyGnome 2025 (https://github.com/HappyGnome)

DISCLAIMER: 
Please don't copy this code, I'm relatively new to Haskell and have probably made many mistakes
of style and logic. This is a learning project for the author, and has been prepared for a timed challenge.

-}

{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module A2502 where

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
        inpPathBase ="app/Puzzles/Input/A2502/"
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
{-@@1-}inpPath = inpPath1         -- Choose Test or Input here 

    ls <- IOH.getFileLines inpPath
    printSoln "Part 1" inpPath $ solve1 $ parseLines ls

--------------------------
-- Entry point for Part 2
exec2 :: String -> String -> String -> IO()
exec2 inpPathBase inpPath0 inpPath1 = do
    let 
        --inpPath2 = inpPathBase ++ "Test2.txt"
 {-@@2-}inpPath = inpPath1         -- Choose Test or Input here 

    ls <- IOH.getFileLines inpPath
    printSoln "Part 2" inpPath $ solve2 $ parseLines ls

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
        pr cs = (\css -> (readInt $ head css, readInt $ css !! 1)) $ splitOnPred (=='-') cs
        ln = splitOnPred (==',')
    in
        map pr $ ln $ concat ls

--------------------------------------------------------------------------------------------
-- Solver

solve1 :: [(Int,Int)] ->  Maybe Int
solve1 plr = -- @@1
    let

    in
        Just $ sum $ map invInRange plr
        
                

solve2 :: [(Int,Int)] -> Maybe Int
solve2 plr = -- @@2
    Just $ sum $ map invInRange2 plr

solveDebug :: [(Int,Int)] ->  IO()
solveDebug plr = do
    return ()
--------------------------------------------------------------------------------------------
-- Business

-- splitDig x = splitDig' (x,0,1)
-- splitDig' (x,y,z)
--     | x <= y = (x,y)
--     | otherwise = splitDig' (x `div` 10, (x `mod` 10) * z + y, z * 10)
-- 
-- isInvalid x = z == y 
--     where
--         (z,y) = splitDig x
--

isInvalid x = 
    let
        (st,nd) = splitAt (length cs `div` 2) $ cs
            where
                cs = show x
    in
        st == nd

isInvalid2 x = 
    let

        testStr cs  = testStr' cs 1

        testStr' cs n
            | 2*n > length cs = False
            | all ((==) $ head $ ptsc) ptsc = True
            | otherwise = testStr' cs (n+1)
            where

                ptsc = pts cs

                pts [] = []
                pts ds = st : pts nd 
                    where
                        (st, nd) = splitAt n ds

                
    in
        testStr $ show x

invInRange (x,y) = sum $ filter isInvalid [x..y]

invInRange2 (x,y) = sum $ filter isInvalid2 [x..y]
