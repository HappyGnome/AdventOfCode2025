{-

Copyright HappyGnome 2025 (https://github.com/HappyGnome)

DISCLAIMER: 
Please don't copy this code, I'm new to Haskell and have probably made many mistakes
of style and logic. This is a learning project for the author.

-}

{-# LANGUAGE DefaultSignatures #-}

module IoHelpers (
        PuzzleSolution
        , getFileLines
        , printSoln
        , readInt
        , splitOnPred
        , splitLinesOnEmpty
        , renderGrid
        , renderMat
        , loadJust2D 
        , runTimed
        , runTimedIO
        , readParseSolve
        , readParseSolve'
        , readParseSolveReps
        , reptest
        , reptestMin) where

import System.Windows.Clipboard
import qualified Data.Map as Map
import MiniLinLib as MLL
import Shorts
import Text.Printf
import Data.Maybe as Maybe
import Data.List.Index
import Data.Tuple.Extra (secondM)
import Data.Time.Clock.System()
import Data.Time.Clock
import Data.Int()
import GHC.Conc
import Control.DeepSeq
-- import System.CPUTime
import System.Win32.Time
-- import Control.Concurrent()
--import Debug.Trace

------------------------------------------------------------------------------
-- Puzzle solution printing
class PuzzleSolution a where
    showSoln :: a -> String

    showSolnList :: [a] -> String
    default showSolnList :: [a] -> String
    showSolnList = concatMap showSoln 

    {-# MINIMAL showSoln #-}

instance (PuzzleSolution a) => PuzzleSolution [a] where
    showSoln = showSolnList

instance PuzzleSolution Int where
    showSoln = show

instance PuzzleSolution Float where
    showSoln = show

instance PuzzleSolution Double where
    showSoln = show

instance PuzzleSolution Integer where
    showSoln = show

instance PuzzleSolution Char where
    showSoln c = [c]
    showSolnList = id

------------------------------------------------------------------------------

getFileLines :: String -> IO [String]
getFileLines path = do
    raw <- readFile path
    return $ lines raw

printSoln :: (PuzzleSolution a) => String -> String -> Maybe a -> IO()
printSoln name inpPath (Just val) = do
    _ <- putStrLn $ "\n@" ++ name ++ " ----------------------------------------- " ++ inpPath
    _ <- putStrLn $ showSoln val
    putStrLn ""
    setClipboardString(showSoln val)

printSoln name inpPath Nothing = do
    _ <- putStrLn $ "\n@" ++ name ++ "-------------------------------------------" ++ inpPath
    _ <- putStrLn "Not Complete"
    putStrLn ""

readInt :: String -> Int
readInt cs = read cs :: Int

splitLinesOnEmpty :: [String] -> [[String]]
splitLinesOnEmpty = splitOnPred (== "")

loadJust2D ::  (b -> Maybe c) -> [[b]] -> Map.Map (V2 Int) c
loadJust2D f lls =   
    let
        kvp = concatMap (\(i,ls) -> mapMaybe (\(j,b) -> secondM f (V2 i j, b)) $ indexed ls) $ indexed lls
    in
        Map.fromList kvp
    --mapMaybe (secondM f) $ concatMap (\(i,ls) -> map (\(j,b) -> (V2 i j, b)) $ indexed ls) $ indexed lls

renderGrid ::  Int -> Int -> (Maybe b -> Char) -> Map.Map (V2 Int) b -> IO()
renderGrid nrow ncol f m = do
    let 
        ls = reverse $ renderGridInner nrow ncol f m

    printf $ unlines ls

renderMat :: (b->String) -> [[b]] -> IO()
renderMat f lss =
    let 
        sss = map (map f) lss
        mxl = (+) 1 $ maximum $ map (maximum.map length) sss
        pss = map (concatMap $ rpad mxl ' ') sss
    in
        printf $ unlines pss

---------------------------------------
-- Timing
runTimed :: (NFData b) => (a -> b) -> a -> IO (b,NominalDiffTime)
runTimed f x = do
     st <- getCurrentTime
     let
         y = f x
     nd <- rnf y `pseq` getCurrentTime
     return (y, diffUTCTime nd st)

runTimed' :: (NFData b, PerfCounter pc) => pc -> (a -> b) -> a -> IO (b,DiffTime)
runTimed' pc f x = do
     st <- tickCount pc
     let
         y = f x
     nd <- rnf y `deepseq` tickCount pc
     return (y, ticksToDiffTime pc st nd)

        
runTimedIO :: (a -> IO b) -> a -> IO (b,NominalDiffTime)
runTimedIO f x = do
    st <- getCurrentTime
    y <- f x
    nd <- getCurrentTime
    return (y, diffUTCTime nd st)
        
---------------------------------------
-- Private
renderRowInner :: Int -> Int -> (Maybe b -> Char) -> Map.Map (V2 Int) b -> String
renderRowInner _ 0 _ _ = ""
renderRowInner nrow ncol f m =
    let 
        char = 
            f $ Map.lookup (V2 (nrow-1)  (ncol-1)) m 
    in
        char : renderRowInner nrow (ncol-1) f m

renderGridInner :: Int -> Int -> (Maybe b -> Char) -> Map.Map (V2 Int) b -> [String]
renderGridInner 0 _ _ _ = []
renderGridInner nrow ncol f m =
    let
        cs = reverse $ renderRowInner nrow ncol f m
    in
        cs : renderGridInner (nrow-1) ncol f m

--------------------------------------------------------------------------------------------------------------
--Running solvers
--
-- | Read input, parse, call solver and print output, plus basic timing
readParseSolve :: (PuzzleSolution b, NFData a, NFData b) => String -> String -> ([String] -> a) -> (a -> Maybe b) -> IO()
readParseSolve name inpPath parse solve = do

    (ls, readTime) <- runTimedIO getFileLines inpPath

    (psd, parseTime) <- runTimed parse ls

    (soln, solveTime ) <- runTimed solve psd

    (_,fullSolveTime) <- runTimedIO (printSoln name inpPath) soln 

    putStrLn $ "Read time: " ++ show readTime
            ++ "  Parse time: " ++ show parseTime
            ++ "  Solve time: " ++ show solveTime
            ++ "  Output time: " ++ show fullSolveTime

-- | Read input, parse, call solver and print output. 
-- No timing is provided, but types involved don't need to be NFData instances
readParseSolve' :: (PuzzleSolution b) => String -> String -> ([String] -> a) -> (a -> Maybe b) -> IO()
readParseSolve' name inpPath parse solve = do

    (ls, readTime) <- runTimedIO getFileLines inpPath

    let
        psd =parse ls

        soln = solve psd

    (_,fullSolveTime) <- runTimedIO (printSoln name inpPath) soln 

    putStrLn $ "Read time: " ++ show readTime 
            ++ "  Output time: " ++ show fullSolveTime

-- | Read input, parse, call solver and print output, plus basic timing
readParseSolveReps :: (PuzzleSolution b, NFData a, NFData b) => String -> String -> ([String] -> a) -> Int -> (a -> Maybe b) -> IO()
readParseSolveReps name inpPath parse nSolve solve = do

    (ls, readTime) <- runTimedIO getFileLines inpPath

    (psd, parseTime) <- runTimed parse ls

    pc <- calibrateQPC 100

    (soln, solveTime ) <- reptestMin nSolve pc solve psd

    (_,fullSolveTime) <- runTimedIO (printSoln name inpPath) soln 

    putStrLn $ "Read time: " ++ show readTime
            ++ "  Parse time: " ++ show parseTime
            ++ "  Min Solve time (n=" ++ show nSolve ++ "): " ++ show solveTime
            ++ "  Output time: " ++ show fullSolveTime


-- | Run a function multiple times on the same input, ignoring all but the last output
reptest :: (NFData b) => Int -> (a->b) -> a -> b
reptest n f x
    | n <= 0 = f x
    | otherwise = f x `deepseq` reptest (n-1) f x 

-- | Run a function multiple times on the same input, ignoring all but the last output. Find the minimum time.
reptestMin :: (NFData b, PerfCounter pc) => Int -> pc -> (a->b) -> a -> IO (b,DiffTime)
reptestMin n pc f x
     | n <= 0 = runTimed' pc f x
     | otherwise = do 
            (b, dt0) <- runTimed' pc f x
            (_, dt1) <- reptestMin (n-1) pc f x
            return (b, min dt0 dt1)

----------------------------------------------
---- TODO : this needs some cleanup. Maybe a separate mini-library?

class PerfCounter a where
    freq :: a -> Integer
    tickCount :: a -> IO Integer

-- | Perf Counter based on SystemTime from ProcessTimes
newtype QPC = QPC {freq_ :: Integer}

instance PerfCounter QPC where
    freq = freq_
    tickCount _ = queryPerformanceCounter    


calibrateQPC :: Int -> IO QPC
calibrateQPC _ = do
--    let
--        qpc = QPC 0
--    st <- tickCount qpc
 --   threadDelay (ms * 1000)
--    nd <- tickCount qpc
    
    pubFreq <-  queryPerformanceFrequency
--    let
--        fq = 1000 * (nd - st) `div` toInteger ms

--    putStrLn $  "QPC estimated freq: " ++ show fq ++ " | Stated freq: " ++ show pubFreq
    return $ QPC pubFreq  -- stated frequency seems to be accurate

ticksToDiffTime :: (PerfCounter a) => a -> Integer -> Integer -> DiffTime
ticksToDiffTime pc st nd =
    picosecondsToDiffTime $ (nd - st) * (10^12 :: Integer) `div` freq pc
    

        
