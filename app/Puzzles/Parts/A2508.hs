{-

Copyright HappyGnome 2025 (https://github.com/HappyGnome)

DISCLAIMER: 
Please don't copy this code, I'm relatively new to Haskell and have probably made many mistakes
of style and logic. This is a learning project for the author, and has been prepared for a timed challenge.

-}

{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module A2508 where

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
problemNumber = "A2508"

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

parseLines :: [String] -> [V3 Int]
parseLines ls = 
   let
        f cs = V3 (css !! 0) (css !! 1) (css !! 2)
            where
                css = map readInt $ splitOnPred (==',') cs

    in 
        map f ls
        

--------------------------------------------------------------------------------------------
-- Solver

solve1 :: [V3 Int] ->  Maybe Int
solve1 plr = -- @@
    let

        allPairsDist = map (\(v,w) -> (v, w , dist2 v w) ) $ makeAllPairs plr

        appendMap k v = Map.alter (\ls -> Just $ v : fromMaybe [] ls) k

        f mp (v,w,d) = appendMap v (w,d) mp

        pairsDist0 = foldl f Map.empty $ take 1000
                            $ sortOn thd3 allPairsDist

        pairsDist v = fromMaybe [] (pairsDist0 Map.!? v)
        
        (edgs, rts) = kruskal pairsDist plr
        
        --em = Map.fromList $ map (\(x,y,z) -> (x,y)) edgs
        --

        g mp mem v w = (appendMap w v $ appendMap v w mp, Set.insert w mem)
        -- | Set.member v mem = (appendMap v w mp, Set.insert w mem)
          --  | otherwise = (appendMap w v mp, Set.insert v mem)

        krEdMp = fst $ foldl (\(mp, mem) (v,w,_) ->  g mp mem v w) (Map.empty, Set.fromList rts) edgs


        circSize mem x =  1 + sum ( map (circSize mem') ls)
            where
                ls = filter (\y -> not $ Set.member y mem) $ fromMaybe [] $ krEdMp Map.!? x
                mem' = Set.insert x mem

        circSizes = take 3 $ sortOn Down $ map (circSize Set.empty ) rts

--  kruskal :: (Ord a, Ord b) => (a -> [(a, b)]) -> [a] -> ([(a, a, b)], [a])
    in
        Just $    product circSizes

solve2 :: [V3 Int] -> Maybe Int
solve2 plr = -- @@
    let

        allPairsDist = map (\(v,w) -> (v, w , dist2 v w) ) $ makeAllPairs plr

        appendMap k v = Map.alter (\ls -> Just $ v : fromMaybe [] ls) k

        f mp (v,w,d) = appendMap v (w,d) mp

        pairsDist0 = foldl f Map.empty
                            $ sortOn thd3 allPairsDist

        pairsDist v = fromMaybe [] (pairsDist0 Map.!? v)
        
        (edgs, rts) = kruskal pairsDist plr
        
        --em = Map.fromList $ map (\(x,y,z) -> (x,y)) edgs
        --

        lngedg = head $ sortOn (Down . thd3) edgs 

        x0 = v3x $ fst3 lngedg
        x1 = v3x $ snd3 lngedg

        g mp mem v w = (appendMap w v $ appendMap v w mp, Set.insert w mem)
        -- | Set.member v mem = (appendMap v w mp, Set.insert w mem)
          --  | otherwise = (appendMap w v mp, Set.insert v mem)

        krEdMp = fst $ foldl (\(mp, mem) (v,w,_) ->  g mp mem v w) (Map.empty, Set.fromList rts) edgs

        h (a,b,_)
            | (length $ krEdMp Map.! a) <= 1 = a
            | otherwise = b


--  kruskal :: (Ord a, Ord b) => (a -> [(a, b)]) -> [a] -> ([(a, a, b)], [a])
    in
        Just $ traceShow lngedg $ x0 * x1
        

solveDebug :: [V3 Int] ->  IO()
solveDebug plr = do
    return ()
--------------------------------------------------------------------------------------------
-- Business


