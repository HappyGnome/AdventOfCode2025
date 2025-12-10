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

        applyBtn n jm b =  foldl (flip (Map.update (\x -> Just (x-n) ))) jm b

        -- Get dimensions from most constrained to least
        grps plr = map head $ sortOn length $ group $ sort $ concat $ btns plr
        grpsM plr= Map.fromList $ mapWithIndex (flip (,)) $ grps plr

        btns'' plr = map (\b -> (b,minimum $ map (grpsM plr Map.!) b)) $ btns plr

        btns' plr = sortOn snd $ btns'' plr

        greedyN btn js = minimum $ mapMaybe (js Map.!? ) btn

--
--        tails [] = []
--        tails (x:xs) = (x:xs) : tails xs
--
--      bfsTree :: (a -> Int -> [a]) -> (a -> Bool) -> [a] -> ([a], Int)
--
--      bfsMem :: (a -> Int -> b -> ([a], b)) -> (a -> b -> Bool) -> [a] -> b -> ([a], Int, b)
--
        dfsf0' jm cost b bs =   map (\n' -> (applyBtn n' jm b,cost+n',bs)) 

        dfsf0 (jm,cost,[]) _ mem = ([],mem') 
            where
                mem' 
                    | dfsf1 jm =  min mem cost
                    | otherwise = mem

        dfsf0 (jm,cost,[(b,_)]) _ mem
            | cost > mem = ([],mem)
            | otherwise = (dfsf0' jm cost b [] [n],mem')
            where
                n = greedyN b jm

                pass = dfsf1 jm
                mem' 
                    | pass =  min mem cost
                    | otherwise = mem

        dfsf0 (jm,cost,bp:bp':bs) _ mem
            | cost > mem = ([],mem) 
            | bw == bw' =  (dfsf0' jm cost b (bp':bs) $ reverse [0..n], mem') -- reverse not needed?
            | otherwise =  (dfsf0' jm cost b (bp':bs) [n], mem') -- Last vector for this head is constrained
            where
                (b,bw) = bp
                (b',bw') = bp'
                n = greedyN b jm
                pass = dfsf1 jm
                mem' 
                    | pass =  min cost mem
                    | otherwise = mem

        dfsf1 jm
            | any (<0) jm = traceShow jm $ error "err 0"
            | otherwise = all (== 0) jm

        doBfs plr = traceShow (btns' plr) $ bfsMem dfsf0 (\ _ _ -> False) [(jToM $ jltg plr, 0, btns' plr)] 999999999

        doBfs' plr = traceShow y $ y
            where 
                y = doBfs plr
    in
        Just $ sum $  map (thd3 . doBfs') plrs

solveDebug :: [ParseLineResult] ->  IO()
solveDebug plr = do
    return ()
--------------------------------------------------------------------------------------------
-- Business



