{-

Copyright HappyGnome 2025 (https://github.com/HappyGnome)

DISCLAIMER: 
Please don't copy this code, I'm relatively new to Haskell and have probably made many mistakes
of style and logic. This is a learning project for the author, and has been prepared for a timed challenge.

-}

{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module A2511 where

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
import qualified Data.Set as Set

import IoHelpers as IOH
import MiniLinLib
import GridStep
import PuzzleAlgorithm 
import Shorts
import Norms
import ArithEx


problemNumber :: String
problemNumber = "A2511"

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
       inpPath2 = inpPathBase ++ "Test3.txt"
 {-@@-}inpPath = inpPath2         -- Choose Test or Input here 

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

parseLines :: [String] -> [(String,[String])]
parseLines ls = 
    let
        toks = splitOnPred(==' ')
        foo (cs:css) = (init cs , css)
    in
        map (foo . toks) ls

--------------------------------------------------------------------------------------------
-- Solver

solve1 :: [(String,[String])] ->  Maybe Int
solve1 plr = -- @@
    let
        mp = Map.fromList plr
        st = "you"
        nd = "out"

        recu' mem css = foldl (\(i,m) cs -> first (+ i) $ recu cs mem) (0,mem) css
        recu cs mem 
            | cs == nd = (1,mem)
            | isJust remd = (fromJust remd,mem)
            | otherwise = (res,mem'')
            where
                remd = mem Map.!? cs
                (res,mem') = recu' mem $ fromMaybe [] $  mp Map.!? cs
                mem'' =  Map.insert cs res mem'

        

--dfsMem :: (a -> Int -> b -> ([a], b)) -> (a -> b -> Bool) -> [a] -> b -> (Maybe (a, Int), b)
--        dfsf0 cs _ mem 
--            | isJust remd = fromJust remd
--            | cs == nd = (1,Map.insert cs 1 mem)
--            | otherwise = (cnt, mem')
--            where
--                remd = mem Map.!? cs
--                cnt = 
--                mem' = Map.insert cs cnt

            
    in
        Just $ fst $ recu st Map.empty

solve2 :: [(String,[String])] -> Maybe Int
solve2 plr = -- @@
    let
        mp = Map.fromList plr
        st = "svr"
        nd = "out"
        inclu = ["dac","fft"]

        recu' mem nd' vstd = foldl (\(i,m) cs -> first (+ i) $ recu cs mem nd' vstd) (0,mem)
        recu cs mem nd' vstd 
            | Set.member cs vstd = trace "Loop!" $ (0,Map.insert cs 0 mem)
            | cs == nd' = (1,mem)
            | isJust remd = (fromJust remd, mem)
            | otherwise =  (res,mem'')
            where
                remd = mem Map.!? cs
                (res,mem') = recu' mem nd' vstd' $ fromMaybe [] $  mp Map.!? cs
                mem'' =  Map.insert cs res mem'
                vstd' = Set.insert cs vstd 

        fft = "fft"
        dac = "dac"

        st2F = trace "st2F" $ fst $ recu st Map.empty fft           Set.empty
        st2D = trace "st2D" $ fst $ recu st Map.empty dac Set.empty
        dac2F =trace "dac2F" $ fst $ recu dac Map.empty fft Set.empty
        dac2N =trace "dac2N" $ fst $ recu dac Map.empty nd Set.empty
        fft2D =trace "fft2D" $ fst $ recu fft Map.empty dac Set.empty
        fft2N =trace "fft2N" $ fst $ recu fft Map.empty nd Set.empty
 
    in
        traceShow [st2F,st2D,dac2F,dac2N,fft2N, fft2D] $ Just $ (st2F * fft2D * dac2N) + (st2D * dac2F * fft2N)

solveDebug :: [(String,[String])] ->  IO()
solveDebug plr = do
    return ()
--------------------------------------------------------------------------------------------
-- Business


