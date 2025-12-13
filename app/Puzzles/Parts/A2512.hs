{-

Copyright HappyGnome 2025 (https://github.com/HappyGnome)

DISCLAIMER: 
Please don't copy this code, I'm relatively new to Haskell and have probably made many mistakes
of style and logic. This is a learning project for the author, and has been prepared for a timed challenge.

-}

{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE TupleSections #-}

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
import System.Random
import Data.Function

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
        inpPath2 = inpPathBase ++ "Test2.txt"
{-@@-}  inpPath = inpPath1         -- Choose Test or Input here 

    --rng <- newRNG
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

data ParseLineResult = ParseLineResult {area :: (Int,Int) , counts :: [(Int,Int)]} deriving (Eq, Show)

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

data Tile = Tile {sqrs ::[V2 Int], nbrs :: [V2 Int]} deriving (Show, Eq, Ord)

type Hints = [V2 Int]
data CountElt = CountElt {tgtCount ::Int, shapes :: [Tile]} deriving (Show, Eq)
--
--instance Ord CountElt where
--   compare = compare `on` (\c -> (tgt c, repTile c)) 

solve1 :: ([[V2 Int]], [ParseLineResult]) ->  Maybe Int
solve1 (vss,plrs) = -- @@
    let
        allShapes = map makeIso vss

        -- Map Int CountElt
        tileCounts plr = Map.mapWithKey (\k v -> CountElt v $ allShapes !! k) $ Map.fromList $ counts plr

        freeSpaceCount (x,y) tcs =
            let
                tcarea (CountElt i ts) = i * length (sqrs $ head ts)
            in
                x * y - sum (Map.map tcarea tcs)

        augTileCounts plr
            | fsc >= 0 = Just  $ Map.insert 1000 (CountElt fsc [Tile [V2 0 0] []]) $ tcs
            | otherwise = Nothing
            where 
                tcs = tileCounts plr
                ar = area plr
                fsc = freeSpaceCount ar tcs
        
        
        searchPath (rows,cols) = [V2 x y | x <- [0..(rows-1)], y <- [0..(cols-1)]]

        -- [V2 Int] -> Set V2 -> Map Int CountElt -> [([V2 Int],Set V2, Map Int CountElt)]
        dfsf0 [] _ _ _ = []
        dfsf0 (x:xs) st cts _ = 
            let
                countElt' (i,CountElt tgt shs)
                    | tgt <= 0 = []
                    | otherwise = map (i, , CountElt (tgt - 1) shs) shs

                tryCEs = concatMap countElt' $ Map.toList cts
                
                fits = filter (\(_,s,_ ) -> canFit st s x ) tryCEs

                place (i, sh, ce) = (xs, placeAt x sh st, Map.insert i ce cts)

                withOptSkip ls
                    | Set.member x st = (xs,st,cts) : ls
                    | otherwise = ls
            in
                withOptSkip $ map place fits

        dfsf1 _ _ = all ((<= 0) . tgtCount)
    
        
            


--        hints0 plr t = initTryAts (area plr) $ sqrs t
--
--        initCe plr (vss', i) = CountElt i (Map.fromList $ map (\t -> (t, hints0 plr t)) vss') $ head vss'
--
--
--        -- Set V2 ->CountElt -> [(Set V2, Maybe CountElt)] -- Empty list => no solution, ce == Nothing => All elts placed for this iso
--        tryCE st (CountElt 0 _ _) = [(st,Nothing)]
--        tryCE st ce =
--            filter ( (||) (tgt'== 0) . isJust . snd )  $ map (\(t,s,h) -> (s , updIsoHint t h )) flat
--            where 
--                isoMp = isoHint ce
--                tgt' = tgt ce - 1
--
--                -- Could also memo-ize on st & counts combinations?
--                
--                -- Map Tile (Set V2,Hints)
--                mp = Map.mapMaybeWithKey (tryFit st) isoMp
--
--                -- Shortcut possible : tgt > hints => fail
--                flat = filter ((<=) tgt' . length . thd3 ) $ map (\(k, (x,y)) -> (k,x,y)) $ Map.toList mp
--
--                updIsoHint _ [] = Nothing
--                updIsoHint t h = Just $ CountElt tgt' (Map.insert t h isoMp) (repTile ce)
--                
--
--        -- Set (V2 Int) -> Map Int CountElt -> Int -> [(Set (V2 Int) ,Map Int CountElt)]
--        procElt st mp i = 
--            let 
--                scs = tryCE st $ mp Map.! i
--            in
--                map (\(st', ce) -> (st', Map.update (const ce) i mp)) scs
--                
--            
--        -- Set (V2 Int) -> Map Int CountElt -> Set (CountElt, Set V2 Int) -> Bool
--        recu st ces
--            | Map.null ces = True
--            | elem [] childs = False
--            | otherwise = any (any (uncurry recu )) childs
--            where
--                kys = Map.keys ces
--
--                childs = map (procElt st ces) kys
                


--        recu _ [] _ = True
--        recu ar ((_,0):tcs) st = recu ar tcs st -- no more times to fit in top shape
--        recu _ (([],_):_) _ = False 
--        recu ar ((tiles, tgtN):tcs) st =
--            let 
--                sts = tryFit' ar st tile
--
--                onSkip = recu ar ((tiles,tgtN):tcs) st
--            in
--                any (recu ar ((tile:tiles,tgtN - 1):tcs)) sts || onSkip
--
        checkPlr plr
            | Nothing <- atc = trace "Trivial case" $ Nothing -- More present tiles than space in the region
            | Just atc' <- atc =  dfSearch (uncurry3 dfsf0) (uncurry3 dfsf1) [(searchPath ar, initSt ar,atc')]
            where
                ar = area plr
                atc = augTileCounts plr


    in
        traceShow plrs $ Just $ length $ mapMaybe checkPlr plrs


solve2 :: ([[V2 Int]], [ParseLineResult]) -> Maybe Int
solve2 (vss,plrs) = -- @@
    Nothing

solveDebug :: ([[V2 Int]], [ParseLineResult]) ->  IO()
solveDebug (vss,plrs) = do
    let
        allShapes = map makeIso vss
        
        toMp vs = Map.fromList $ map (\v -> (v,True)) vs

        maxX tile = maximum ( map v2x tile) + 1
        maxY tile = maximum ( map v2y tile) + 1

        ren Nothing = '.'
        ren _ = '#'

        doRender (Tile vs _) = renderGrid (maxX vs) (maxY vs) ren (toMp vs)

        -- use mapM_ ?
        doRender' i0 vss' = foldl (\i0 vs -> i0 >> doRender vs >> putStrLn " ") i0 vss'
        doRender'' vsss i0 = foldl (\i0 vss' -> i0 >> doRender' i0 vss' >> putStrLn " ------------------ ") i0 vsss
        

    --renderGrid ::  Int -> Int -> (Maybe b -> Char) -> Map.Map (V2 Int) b -> IO()
    traceShow (sum $ map length allShapes) $ doRender'' allShapes (return())
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
        y0 = minimum $ map v2y $ filter ((== x0) . v2x) vs

    in
        map (`sub` V2 x0 y0) vs

mkTile :: [V2 Int] -> Tile
mkTile vs = 
    let
        nbhd = Set.fromList $ concatMap (\v -> map (`stepInDir8` v ) [Up,Dn,Lt,Rt] ) vs
    in
        Tile vs $ Set.toList $ Set.difference nbhd $ Set.fromList vs

makeIso :: [V2 Int] -> [Tile]
makeIso vs =
    let
        rots' = iterate (rot90 .) 
        rots = take 4 (rots' id) ++ take 4 ( rots' flipx)

        doRot vs' r = sort $ recentre $ map r vs'

        tsraw =  nub $ map (doRot vs) rots -- Account for symmetries
    in
         map mkTile tsraw  



tryFit :: Set.Set (V2 Int) -> Tile -> Hints -> Maybe (Set.Set (V2 Int), Hints)
tryFit st tile hints
    | [] <- hints' = Nothing
    | (h:hs) <- hints' = Just (placeAt h tile st , hs)
    where
        hints' ={- sortOn (Down . contact st tile) $-} filter (canFit st tile) hints 
 --   in
        -- Probably not worth deleting h form the list here
 --       traceShow (tile,length hints) $ map (\h -> (placeAt h tile st , hints')) hints' -- Return a list in case we can add a useful ordering later

contact :: Set.Set (V2 Int) -> Tile -> V2 Int -> Int
contact st (Tile _ nbs) v = length $ filter (`Set.member` st) $ map (`add` v) $ nbs


canFit :: Set.Set (V2 Int) -> Tile -> V2 Int -> Bool
canFit st (Tile tile _) v = not $  any ((`Set.member` st) . (`add` v)) tile 

placeAt :: V2 Int -> Tile -> Set.Set (V2 Int) -> Set.Set (V2 Int)
placeAt (V2 x y) (Tile vs _) st = Set.union st $ Set.fromList $ map (`add` V2 x y) vs

initTryAts :: (Int,Int) -> [V2 Int] -> [V2 Int]
initTryAts (rows,cols) tile =
    let 
        maxX = rows - 1 - maximum (map v2x tile)
        maxY = cols - 1 - maximum (map v2y tile)
    in
        [V2 x y | x <- [0..maxX], y <- [0..maxY]]

initSt :: (Int,Int) -> Set.Set (V2 Int)
initSt (rows,cols)  = 
    let
        rrng = [-1..rows]
        crng = [-1..cols]
    in
        Set.fromList $ [V2 (-1) y | y <- crng] ++ [V2 rows y | y <- crng] ++  [V2 x (-1) | x <- rrng] ++  [V2 x cols | x <- rrng]

easierProblems :: [ParseLineResult] -> ParseLineResult -> Int
easierProblems plrs plr =    
    let
        countLess mp (k,v) 
            | Nothing <- z = v <= 0
            | Just z' <- z = v <= z' 
            where
                z = mp Map.!? k

        areaLessEq (x0,y0) (x1,y1) = (max x0 y0 <= max x1 y1 ) && (min x0 y0 <= min x1 y1)

        isEasier p0 p1 = (all ( countLess $ Map.fromList $ counts p1) $ counts p0) && areaLessEq (area p1) (area p0)
    in
        length $ filter (`isEasier` plr) plrs

harderProblems :: [ParseLineResult] -> ParseLineResult -> Int
harderProblems plrs plr =    
    let
        countLess mp (k,v) 
            | Nothing <- z = v <= 0
            | Just z' <- z = v <= z' 
            where
                z = mp Map.!? k

        areaLessEq (x0,y0) (x1,y1) = (max x0 y0 <= max x1 y1 ) && (min x0 y0 <= min x1 y1)

        isEasier p0 p1 = (all ( countLess $ Map.fromList $ counts p1) $ counts p0) && areaLessEq (area p1) (area p0)
    in
        length $ filter (isEasier plr) plrs


{-
 State:
    - [([(Tile,hint)], countLeft)] mem st
    - mem = Set (set, counts)
    -
Alg:
    - take tileIso with +ve count
    - try to place it using hint
    - return Nothing & mem' or Just updated hint, st , count & mem'
    - if Nothing returned -> back a level 
    - if Just, recurse with new hint, st and count
 -}

 -- TODO: memo-ize between rows?
