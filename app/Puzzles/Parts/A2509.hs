{-

Copyright HappyGnome 2025 (https://github.com/HappyGnome)

DISCLAIMER: 
Please don't copy this code, I'm relatively new to Haskell and have probably made many mistakes
of style and logic. This is a learning project for the author, and has been prepared for a timed challenge.

-}

{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module A2509 where

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
problemNumber = "A2509"

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
--
parseLines :: [String] -> [V2 Int]
parseLines ls = 
    let 
        mpln cs = V2 (readInt $ head css) (readInt $ css !! 1 )
            where 
                css = splitOnPred (==',') cs
    in
        map mpln ls

--------------------------------------------------------------------------------------------
-- Solver

solve1 :: [V2 Int] ->  Maybe Int
solve1 plr = -- @@
    let 
        prs = makeAllPairs plr
        szpr (V2 x y,V2 x' y') = (1 + abs (x - x')) * (1 + abs (y -y'))
    in
        Just $ maximum $ map szpr prs

solve2 :: [V2 Int] -> Maybe Int
solve2 plr = -- @@
    let 
        prs = makeAllPairs plr
        szpr (V2 x y,V2 x' y') = (1 + abs (x - x')) * (1 + abs (y -y'))

        prs' = sortOn (Down . szpr) prs

        plr' = plr ++ [head plr]



        lineNbhd acc (V2 x y) (V2 x' y')
            | x == x' = [V2 x z | z <- itvl y y'] ++ [V2 (x+ 1) z | z <- itvl y y'] ++ [V2 (x-1) z | z <- itvl y y'] ++ acc
            | y == y' = [V2 z y | z <- itvl x x'] ++ [V2 z (y+1) | z <- itvl x x'] ++ [V2 z (y-1) | z <- itvl x x'] ++acc
            | otherwise = traceShow (x,y,x',y') $ error "Diagonal line!"
            where
                itvl a b = [(min a b)..(max a b)]

        pathNbhd = foldl2x1 lineNbhd [] plr'

        badCells = traceShow (length pathNbhd) $ filter (not . insideShape plr) pathNbhd

        checkR (V2 x0 y0, V2 x1 y1) =
             not $ any (insideRect) badCells
            where
                insideRect (V2 x y) = inItvl x0 x1 x && inItvl y0 y1 y 
                inItvl a b x = (a <= x && x <= b) || (b <= x && x <= a)

        doFind [] = 0
        doFind (p:ps)
            | checkR p = traceShow p $ szpr p
            | otherwise = doFind ps
            
            
    in
        Just $ doFind prs'

solveDebug :: [V2 Int] ->  IO()
solveDebug plr = do
    return ()
--------------------------------------------------------------------------------------------
-- Business

insideShape :: [V2 Int] -> V2 Int -> Bool
insideShape vs (V2 x y) =
    let
        region w
            | x' > 0 && y'> 0 = Ur
            | x' > 0 && y'< 0 = Dr
            | x' < 0 && y'> 0 = Ul
            | x' < 0 && y'< 0 = Dl
            | x' == 0 && y'> 0 = Up
            | x' == 0 && y'< 0 = Dn
            | x' < 0 && y' == 0 = Lt
            | otherwise = Rt
            where
                (V2 x' y') = w `sub` V2 x y

        regDiff r0 r1
            | r0 == Dl && r1 == Dr = 1
            | r0 == Dl && r1 == Ul = -1
            | r0 == Dr && r1 == Dl = -1
            | r0 == Dr && r1 == Ur = 1
            | r0 == Ul && r1 == Ur = -1
            | r0 == Ul && r1 == Dl = 1
            | r0 == Ur && r1 == Ul = 1
            | r0 == Ur && r1 == Dr = -1
            | r0 == Ul && r1 == Dr = error "err0"
            | r0 == Dr && r1 == Ul = error "err1"
            | r0 == Dl && r1 == Ur = error "err2"
            | r0 == Ur && r1 == Dl = error "err2"
            | otherwise = 0

        recu cnt rgn [] = cnt /= 0
        recu cnt rgn (v:ws)
            | v == V2 x y = True
            | (max rgn rgn', min rgn rgn') == (Dn,Up)  = True
            | (max rgn rgn', min rgn rgn') == (Rt,Lt)  = True
            | otherwise = recu (cnt + regDiff rgn rgn') rgn' ws
            where 
                rgn' = region v
    in
        recu (0:: Int) (region $ head vs) vs
