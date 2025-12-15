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
import Memoize


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
    let 
        --inpPath2 = inpPathBase ++ "Test2.txt"
{-@@-}inpPath = inpPath1         -- Choose Test or Input here 

    readParseSolve' (problemNumber ++ " / Part 1") inpPath parseLines solve1
    -- 447

--------------------------
-- Entry point for Part 2
exec2 :: String -> String -> String -> IO()
exec2 inpPathBase inpPath0 inpPath1 = do
    let 
        --inpPath2 = inpPathBase ++ "Test2.txt"
 {-@@-}inpPath = inpPath1         -- Choose Test or Input here 

    readParseSolve' (problemNumber ++ " / Part 2") inpPath parseLines solve2
    --18960

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

solve1 ::  [ParseLineResult] ->  Maybe Int
solve1 plrs = -- @@
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
        
        mapWithIndex f xs = reverse $ fst $ foldl' (\(acc,i) x -> (f i x : acc , i+1)) ([],0)  xs

        jToM js = Map.fromList $ mapWithIndex (,) js -- Go from tgt to 0

        applyBtn n  =  foldl' (flip (Map.update (\x -> Just (x-n) )))
        applyBtn1 =  foldl' (flip (Map.update (\x -> Just (x-1) )))

        memoChoice' = toMemoized2 (binomialC :: (Int -> Int -> Int)) 
        memoChoice = facBranchIn memoChoice' (\(x,y,z) -> ((y,z),(x,y,z))) :: GenFactory (Memoized2 Int Int Int) (Int,Int,Int) (Memoized2 Int Int Int, Int)

        -- Get dimensions from most constrained to least

        -- [[Int]] -> [(Int,Int)]   --(indx, btn dof)
        grps bs = map (\xss -> (head xss,length xss)) $ group $ sort $ concat bs

        costs0 jm bs =  map (\(idx,dof) -> (idx,  fromMaybe 0 (jm Map.!? idx), dof)) $ grps bs         

        -- Sort costs using momo-ized choice function
        costs jm bs mc = second (sortOn fst) $ mapFactoryr mc $ costs0 jm bs

        splitMatchesWith f g xs = (map g $ filter f xs, filter (not . f) xs)

       -- btns'' plr = traceShow ("TS1",grpsM plr) $ fst $ foldl' (\(acc,bs) (idx,k) -> first (acc++) $ splitMatchesWith (elem k) (,idx,k) bs) ([] ,btns plr) $ grpsM plr -- map (\b -> (b,minimum $ map (grpsM plr Map.!) b)) $ btns plr

     --   btns' plr = map (\(b,_,k) -> (b,k)) $ sortOn snd3 $ btns'' plr

        -- Max n.o. times we can press this button
        greedyN btn js = minimum $ mapMaybe (js Map.!? ) btn

        --  if known to be constrained, take exactly this many steps to satisfy the constraint (if it's <= greedyN) 
       -- consN (_,bk) js = fromMaybe 0 $ js Map.!? bk
        

        -- Get groups of indexes
        -- calc minimal "complexity" index (n + m - 1 C m - 1) where n = tgt number for the index, m = number of btns wth index
        -- get all btns with "least complex" index, Get all valid combos (i.e. w/o violating other constraints)
        -- for each valid combo, subtract from tgt and repeat with remaining btns
        
        isSoln = all (==0)

        minOn f x y
            | y' < x' = y
            | otherwise = x
            where 
                x' = f x
                y' = f y

        -- Map Int Int -> Int -> [[Int]] -> ??* -> Int  *Whatever bestSearch needs
        recu jm k [] css mc
            | isSoln jm = (0,mc)
            | fromMaybe 1 (jm Map.!? k) /= 0 = (99999999,mc)
            | cs' == [] = (9999999999,mc)
            | otherwise = {-traceShow ("next Key",k',jm,cs',css') $-} recu jm k' cs' css' mc'
            where 
                (k', cs' , css', mc') =  bestSearch jm css mc

        recu jm k [b] css mc = {-trace "P1" $-} (n + res0,mc') -- Constrained choice, take greedily, see if it's enough
            where 
                jm' = applyBtn n jm b
                n = greedyN b jm
                (res0,mc') = recu jm' k [] css mc

        recu jm k (b:bs) css mc
            | any(<0) jm = (9999999999,mc)
            |otherwise = {- trace "P2" $-} minOn fst (res0 ,mc') (first (+1) $ recu jm' k (b:bs) css mc') -- Try changing order here?
            where 
                (res0,mc') = recu jm k bs css mc
                jm' = applyBtn1 jm b

        
        -- css =  [btns]
        bestSearch jm [] mc = (0,[],[],mc)
        bestSearch jm [b] mc = (head b,[b],[],mc)

        bestSearch jm bs mc = {-traceShow (k,bs',cs) $-} (k,bs',cs,mc')
            where
                (mc',csts) = costs jm bs mc
                k = {-traceShow csts $-} fst3 $ snd $ head csts
                (bs',cs) = splitMatchesWith (elem k) id bs

        
        doSrch mc plr = traceShow plr $ swap $ recu jm k bs' cs mc'
            where
                jm = jToM $ jltg plr
                bs = btns plr
                (k,bs', cs, mc' ) = bestSearch jm bs mc

    in
        Just $ sum $ snd $  mapFactoryr (GenFactory doSrch memoChoice) plrs


solveDebug :: [ParseLineResult] ->  IO()
solveDebug plr = do
    return ()
--------------------------------------------------------------------------------------------
-- Business
-- TODO: Memoized and Mapfold separate files. ILP library with the day 10 approach? dfs with ancestry?



