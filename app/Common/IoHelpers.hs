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
        , readParseSolve') where

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
-- import Control.Concurrent()

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
        
runTimedIO :: (a -> IO b) -> a -> IO (b,NominalDiffTime)
runTimedIO f x = do
    st <- getCurrentTime
    y <- f x
    nd <- getCurrentTime
    return (y, diffUTCTime nd st )
        
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

-- | Read input, parse, call solver and print output, plus basic timing
readParseSolve' :: (PuzzleSolution b) => String -> String -> ([String] -> a) -> (a -> Maybe b) -> IO()
readParseSolve' name inpPath parse solve = do

    (ls, readTime) <- runTimedIO getFileLines inpPath

    let
        psd =parse ls

        soln = solve psd

    (_,fullSolveTime) <- runTimedIO (printSoln name inpPath) soln 

    putStrLn $ "Read time: " ++ show readTime 
            ++ "  Output time: " ++ show fullSolveTime
