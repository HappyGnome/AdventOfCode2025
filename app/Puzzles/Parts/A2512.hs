{-

Copyright HappyGnome 2025 (https://github.com/HappyGnome)

DISCLAIMER: 
Please don't copy this code, I'm relatively new to Haskell and have probably made many mistakes
of style and logic. This is a learning project for the author, and has been prepared for a timed challenge.

-}

{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

--{-# LANGUAGE TupleSections #-}

module A2512 where

import Text.Read
import Data.Maybe
import Data.Tuple
import Data.Tuple.Extra
import Debug.Trace
import qualified Data.Map as Map
import qualified Data.Set as Set
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
problemNumber = "A2512"

-- Entry point. Control which steps to run here
exec :: IO()
exec = do
    let 
        inpPathBase ="app/Puzzles/Input/" ++ problemNumber ++ "/"
        inpPath0 = inpPathBase ++ "Test1.txt"
        inpPath1 = inpPathBase ++ "Input1.txt"

    execDebug inpPathBase inpPath0 inpPath1
    exec1 inpPathBase inpPath0 inpPath1
    exec2 inpPathBase inpPath0 inpPath1

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
 {-@@-}inpPath = inpPath0         -- Choose Test or Input here 

    readParseSolve' (problemNumber ++ " / Part 2") inpPath parseLines solve2

--------------------------
-- Entry point for optional debug ops
execDebug :: String -> String -> String -> IO()
execDebug inpPathBase inpPath0 inpPath1 = do
    let 
        --inpPath2 = inpPathBase ++ "Test2.txt"
        inpPath = inpPath1         -- Choose Test or Input here 

    ls <- IOH.getFileLines inpPath
    solveDebug $ parseLines ls

--------------------------------------------------------------------------------------------
--IO

data ParseLineResult = ParseLineResult {area :: (Int,Int) , counts :: [(Int,Int)]} --- Best to replace ParseLineResult with the actual type, if it's simple enough.
                            -- Defining this here just to allow us to warm up the compilet on the blank file

parseLines :: [String] -> ([[V2 Int]], [ParseLineResult])
parseLines ls = 
    let
        blks = IOH.splitLinesOnEmpty ls
        ls' = last blks

        procChar c 
            | c == '#' = Just True
            | otherwise = Nothing

        -- [String] -> [V2 Int]
        procShp = Map.keys . loadJust2D procChar

        shapes = map (procShp . tail) $ init blks

        procLn0 = (\xs -> (readInt $ head xs, readInt $ init $ xs !! 1)) . splitOnPred(=='x') . head . splitOnPred(==' ') 
        procLn1 = filter ((> 0) . snd) . mapWithIndex (\ i cs -> (i, readInt cs)) . tail . splitOnPred(==' ') 

        procLn cs = trace cs $ ParseLineResult (procLn0 cs) (procLn1 cs)
    in
        (shapes, map procLn ls')


--------------------------------------------------------------------------------------------
-- Solver

solve1 :: ([[V2 Int]], [ParseLineResult]) ->  Maybe Int
solve1 (vss,plrs) = -- @@
    let
        shps = map makeIso vss

        -- [([[V2 Int]], Int)]
        tileCounts plr = map (first $ (!!) shps) $ counts plr

        tryFit' = uncurry tryFit
        
        recu _ [] _ = True
        recu ar ((_,0):tcs) st = recu ar tcs st -- no more times to fit in top shape
        recu _ (([],_):_) _ = False 
        recu ar ((tile:tiles, tgtN):tcs) st =
            let 
                sts = tryFit' ar st tile

                onSkip = recu ar ((tiles,tgtN):tcs) st
            in
                any (recu ar ((tile:tiles,tgtN - 1):tcs)) sts || onSkip

        checkPlr plr = 
            recu (area plr) (tileCounts plr) Set.empty
    in
        Just $ length $ filter id $ map checkPlr plrs


solve2 :: ([[V2 Int]], [ParseLineResult]) -> Maybe Int
solve2 (vss,plrs) = -- @@
    Nothing

solveDebug :: ([[V2 Int]], [ParseLineResult]) ->  IO()
solveDebug (vss,plrs) = do
    let
        shps = map makeIso vss
        
        toMp vs = Map.fromList $ map (\v -> (v,True)) vs

        maxX tile = maximum ( map v2x tile) + 1
        maxY tile = maximum ( map v2y tile) + 1

        ren Nothing = '.'
        ren _ = '#'

        doRender vs = renderGrid (maxX vs) (maxY vs) ren (toMp vs)

        -- use mapM_ ?
        doRender' i0 vss' = foldl (\i0 vs -> i0 >> doRender vs >> putStrLn " ") i0 vss'
        doRender'' vsss i0 = foldl (\i0 vss' -> i0 >> doRender' i0 vss' >> putStrLn " ------------------ ") i0 vsss
        

    --renderGrid ::  Int -> Int -> (Maybe b -> Char) -> Map.Map (V2 Int) b -> IO()
    traceShow (sum $ map length shps) $ doRender'' shps (return())
    return ()
--------------------------------------------------------------------------------------------
-- Business

mapWithIndex :: (Int -> a  -> b) -> [a] -> [b]
mapWithIndex f xs = reverse $ fst $ foldl (\(acc,i) x -> (f i x : acc , i+1)) ([],0)  xs

rot90 :: V2 Int -> V2 Int
rot90 (V2 x y) = V2 y (-x)

flipx :: V2 Int -> V2 Int
flipx (V2 x y) = V2 (-x) y

recentre :: [V2 Int] -> [V2 Int] 
recentre [] = []
recentre vs =
    let
        x0 = minimum $ map v2x vs
        y0 = minimum $ map v2y vs
    in
        map (`sub` V2 x0 y0) vs

makeIso :: [V2 Int] -> [[V2 Int]]
makeIso vs =
    let
        rots' = iterate (rot90 .) 
        rots = take 4 (rots' id) ++ take 4 ( rots' flipx)

        doRot vs' r = sort $ recentre $ map r vs'
    in
        nub $ map (doRot vs) rots -- Account for symmetries


tryFit :: Int -> Int -> Set.Set (V2 Int) -> [V2 Int] -> [Set.Set (V2 Int)]
tryFit rows cols st tile =
    let 
        maxX = rows - 1 - maximum ( map v2x tile)
        maxY = cols - 1 - maximum ( map v2y tile)

        toTryAts = [(x,y) | x <- [0..maxX], y <- [0..maxY]]

        tryAt (x,y) = not . any ((`Set.member` st) .(`add` V2 x y)) 
        placeAt (x,y) = Set.union st . Set.fromList . map (`add` V2 x y)

        tryPlaceAt acc xy 
            | tryAt xy tile = placeAt xy tile : acc
            | otherwise = acc
    in
        foldl tryPlaceAt [] toTryAts
