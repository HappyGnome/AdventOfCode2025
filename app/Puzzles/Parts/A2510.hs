{-

Copyright HappyGnome 2025 (https://github.com/HappyGnome)

DISCLAIMER: 
Please don't copy this code, I'm relatively new to Haskell and have probably made many mistakes
of style and logic. This is a learning project for the author, and has been prepared for a timed challenge.

-}

{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module A2510 where

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
import System.Random
import qualified Data.Set as Set

import IoHelpers as IOH
import MiniLinLib
import GridStep
import PuzzleAlgorithm 
import Shorts
import Norms
import ArithEx


problemNumber :: String
problemNumber = "A2510"

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
    rng <- newRNG
    let 
        --inpPath2 = inpPathBase ++ "Test2.txt"
{-@@-}inpPath = inpPath1         -- Choose Test or Input here 

    readParseSolve' (problemNumber ++ " / Part 1") inpPath parseLines (solve1 rng)

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

data ParseLineResult = ParseLineResult {lgts :: [Bool], btns :: [[Int]], jltg ::[Int]} deriving (Show)

parseLines :: [String] -> [ParseLineResult]
parseLines ls = 
    let
        toks = splitOnPred (==' ')
        lgts0 = map (=='#') . init.tail.head . toks
        btns0 = map (map readInt . splitOnPred (==',').init.tail ) . init . tail . toks
        jltg0 = map readInt . splitOnPred (==',') . init . tail . last . toks
    in
        map (\cs -> ParseLineResult (lgts0 cs) (btns0 cs) (jltg0 cs)) ls


    

--------------------------------------------------------------------------------------------
-- Solver

solve1 :: (RandomGen g) => g -> [ParseLineResult] ->  Maybe Int
solve1 rng plrs = -- @@
   -- dfSearch :: (a -> Int -> [a]) -> (a -> Bool) -> [a] -> Maybe (a, Int)
   -- permuteRand :: (RandomGen g) => g -> [a] -> (g,[a])
-- bfsMem :: (a -> Int -> b -> ([a], b)) -> (a -> b -> Bool) -> [a] -> b -> ([a], Int, b)
    let 
        lgts0 = False : lgts0
        --lgtsEq = all (uncurry (==)) . zip
        
        mapWithIndex f xs = reverse $ fst $ foldl (\(acc,i) x -> (f i x : acc , i+1)) ([],0)  xs

        lToM l = Map.fromList $ mapWithIndex (\i b -> (i,not b)) l -- Go from not lgts to all true instead


        applyBtn = foldl (\acc i -> Map.update (Just . not) i  acc) 

        bfsf0 bs lm _ mem 
            | Set.member lm mem = ([],mem)
            | otherwise = (map (applyBtn lm) bs, Set.insert lm mem)
 
        bfsf1 lm _ = and lm

        doBfs plr = bfsMem (bfsf0 $ btns plr) bfsf1 [lToM $ lgts plr] Set.empty
    in
        Just $ sum $  map (snd3 . doBfs) plrs

solve2 :: [ParseLineResult] -> Maybe Int
solve2 plrs = -- @@
    let 

   -- dfSearch :: (a -> Int -> [a]) -> (a -> Bool) -> [a] -> Maybe (a, Int)
        lgts0 = False : lgts0
        --lgtsEq = all (uncurry (==)) . zip
        
        mapWithIndex f xs = reverse $ fst $ foldl (\(acc,i) x -> (f i x : acc , i+1)) ([],0)  xs

        jToM js = Map.fromList $ mapWithIndex (,) js -- Go from tgt to 0

        applyBtn n = foldl (flip (Map.update (\x -> Just (x-n) ))) 

        btns' plr = sortOn (Down . length) $ btns plr

        greedyN btn js = minimum $ mapMaybe (js Map.!? ) btn


--
--        tails [] = []
--        tails (x:xs) = (x:xs) : tails xs

        dfsf0 (jm,cost,[]) _ = [] 
        dfsf0 (jm,cost,b:bs) _ = traceShow(jm,cost,b)$ map (\n' -> (applyBtn n' jm b,cost+n',bs)) $ reverse [0..n]
            where
                n = greedyN b jm

        dfsf1 (jm,cost,bs)
            | any (<0) jm = error "err 0"
            | otherwise = all (== 0) jm

        doDfs plr = traceShow (btns' plr) $ dfSearch dfsf0 dfsf1 [(jToM $ jltg plr, 0, btns' plr)]
    in
        Just $ sum $  mapMaybe (fmap (snd3 . fst) . doDfs) plrs

solveDebug :: [ParseLineResult] ->  IO()
solveDebug plr = do
    return ()
--------------------------------------------------------------------------------------------
-- Business



