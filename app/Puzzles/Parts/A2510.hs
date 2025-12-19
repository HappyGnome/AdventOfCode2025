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
--import Memoize


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
       inpPath2 = inpPathBase ++ "Test2.txt"
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


        applyBtn = foldl (flip (Map.update (Just . not))) 

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

        applyBtn n =  foldl' (flip (Map.update (\x -> Just (x-n) ))) 
        applyBtn1  =  foldl' (flip (Map.update (\x -> Just (x-1) )))

        -- Get dimensions from most constrained to least

        -- [[Int]] -> [(Int,Int)]   --(indx, btn dof)
        grps bs = map (\xss -> (head xss,length xss)) $ group $ sort $ concat bs

        costs0 jm bs =  map (\(idx,dof) -> (idx,  fromMaybe 0 (jm Map.!? idx), dof)) $ grps bs         

        -- Sort costs using memo-ized choice function
        costs jm bs = sortOn snd $ map (\(x,y,z) -> (x,binomialC (y+z-1) (z-1))) $ costs0 jm bs

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

        --dfsMem :: (a -> Int -> b -> ([a], b)) -> (a -> b -> Bool) -> [a] -> b -> (Maybe (a, Int), b)
        
        dfsf0 (jm,k, [],css, csta) _ cstm
            | isSoln jm = ([],min csta cstm)
            | fromMaybe 1 (jm Map.!? k) /= 0 = ([],cstm)
            | null cs' = ([], cstm)
            | otherwise = ([(jm,k',cs',css', csta)],cstm)
            --{-traceShow ("next Key",k',jm,cs',css') $-} recu jm k' cs' css' mc'
            where 
                (k', cs' , css') =  bestSearch jm css

        dfsf0 (jm,k, [b],css, csta) _ mc = ([(jm', k, [], css, n + csta)],mc) -- Constrained choice, take greedily, see if it's enough
            where 
                jm' = applyBtn n jm b
                n = greedyN b jm

        dfsf0 (jm,k, b:b':bs,css, csta) _ cstm
            | cstm < csta = ([],cstm)
            | any(<0) jm = ([],cstm)
            | otherwise =  {-trace "P2" $-} (childs, cstm)
            where 
                jm0 = applyBtn1 jm b
                jm1 = applyBtn1 jm b'
                csta' = 1 + csta
               -- n = greedyN b jm
                childs =  [(jm,k,bs,css,csta),(jm1,k,b':bs,css,csta'),(jm0,k,b:b':bs,css, csta')]--map (\m' -> (jm' m',k,bs,css,m'+ csta)) [0..n]

        dfsf1 _ _ = False
 
        -- css =  [btns]
        bestSearch jm [] = (0,[],[])
        bestSearch jm [b] = (head b,[b],[])

        bestSearch jm bs= (k,sortOn (Down . length) bs',cs) -- put the largest button last (the last in the set is explored greedily first)
            where
                csts = costs jm bs
                k =  fst $ head csts
                (bs',cs) = splitMatchesWith (elem k) id bs

        doSrch plr = traceShow (plr,ans) ans
            where
                jm = jToM $ jltg plr
                bs = btns plr
                (k,bs', cs) = bestSearch jm bs
                (_,ans) =  dfsMem dfsf0 dfsf1 [(jm, k, bs', cs,0)] 999999999999

        mapSrch = foldl' (\acc x -> doSrch x : acc ) []
    in
        Just $ sum $ mapSrch plrs


solveDebug :: [ParseLineResult] ->  IO()
solveDebug plr = do
    return ()
--------------------------------------------------------------------------------------------
-- Business
-- TODO: Memoized and Mapfold separate files. ILP library with the day 10 approach? dfs with ancestry?



