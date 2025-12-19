{-

Copyright HappyGnome 2025 (https://github.com/HappyGnome)

DISCLAIMER: 
Please don't copy this code, I'm new to Haskell and have probably made many mistakes
of style and logic. This is a learning project for the author.

-}

module Test_PuzzleAlgorithm where

import PuzzleAlgorithm
import Text.Printf
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import Data.List
import Data.Tuple.Extra

resultDetails :: Bool -> String -> String
resultDetails b cs =
    if b then
        "OK"
    else
        "Fail <= " ++ cs
------------------------------------------------------------------------------------------
kruskalTest :: IO()
kruskalTest = 
    let
        res = foldl (flip (:)) [] [kruskalTest0,kruskalTest1, kruskalTest2]

    in
        printf $ unlines $ map (\(a,b,c) -> a ++ ": " ++ resultDetails b c) res

kruskalTest0 :: (String,Bool,String)
kruskalTest0 =
    let
        seed = [1,2,3,4,5,6,7,8]
        
        f x
            | x == 1 = []
            | otherwise = [(1,x)]

        (edg,cmp) = kruskal f seed

        pass = length cmp == 1 && length edg == 7

        err = show (edg,cmp)
    in
        ("kruskalTest0", pass, err)

kruskalTest1 :: (String,Bool,String)
kruskalTest1 =
    let
        seed = [1,2,3,4,5,6,7,8]

        edglkp = Map.fromList [(1,[(2,1)]),(3,[(4,2)]),(5,[(6,2)]),(7,[(8,9)]),(9,[(10,1)])]
        
        f x = fromMaybe [] $ edglkp Map.!? x

        (edg,cmp) = kruskal f seed

        pass = length cmp == 4 && length edg == 4 && (sum $ map thd3 edg) == 14

        err = show (edg,cmp)
    in
        ("kruskalTest1", pass, err)

kruskalTest2 :: (String,Bool,String)
kruskalTest2 =
    let
        seed = [1,2,3,4,5,6,7,8]

        edglkp = Map.fromList [(1,[(2,1)]),(2,[(8,1)]), (3,[(4,2)]),(5,[(6,2)]),(7,[(8,9)]),(9,[(10,1)])]
        
        f x = fromMaybe [] $ edglkp Map.!? x

        (edg,cmp) = kruskal f seed

        pass = length cmp == 3 && length edg == 5 && (sum $ map thd3 edg) == 15

        err = show (edg,cmp)
    in
        ("kruskalTest2", pass, err)
------------------------------------------------------------------------------------------
dfTest :: IO()
dfTest = 
    let
        res = foldl (flip (:)) [] [dfTest0,dfTest1, dfTest2]

    in
        printf $ unlines $ map (\(a,b,c) -> a ++ ": " ++ resultDetails b c) res

dfTest0 :: (String,Bool,String)
dfTest0 =
    let
        seed = [1]

        edglkp = Map.fromList [(1,[3]),(3,[9]),(5,[6]),(7,[8]),(9,[10])]
        
        f x lvl = fromMaybe [] $ edglkp Map.!? x

        res = dfSearch f (== 9) seed

        (fnd,atlvl) = fromMaybe (0,0) res
        
        pass = isJust res && fnd == 9 && atlvl == 2

        err = show res
    in
        ("dfTest0", pass, err)

dfTest1 :: (String,Bool,String)
dfTest1 =
    let
        seed = [1]

        edglkp = Map.fromList [(1,[3]),(3,[4]),(5,[6]),(7,[8]),(9,[10])]
        
        f x lvl = fromMaybe [] $ edglkp Map.!? x

        res = dfSearch f (== 9) seed

        (fnd,atlvl) = fromMaybe (0,0) res
        
        pass = isNothing res

        err = show res
    in
        ("dfTest1", pass, err)

dfTest2 :: (String,Bool,String)
dfTest2 =
    let
        seed = [1,9]

        edglkp = Map.fromList [(1,[3]),(3,[4]),(5,[6]),(7,[8]),(9,[10])]
        
        f x lvl = fromMaybe [] $ edglkp Map.!? x

        res = dfSearch f (== 10) seed

        (fnd,atlvl) = fromMaybe (0,0) res
        
        pass = isJust res && fnd == 10 && atlvl == 1

        err = show res
    in
        ("dfTest2", pass, err)
------------------------------------------------------------------------------------------
dfMemTest :: IO()
dfMemTest = 
    let
        res = foldl (flip (:)) [] [dfMemTest0,dfMemTest1] --, dfMemTest2]

    in
        printf $ unlines $ map (\(a,b,c) -> a ++ ": " ++ resultDetails b c) res

dfMemTest0 :: (String,Bool,String)
dfMemTest0 =
    let
        seed = [1]

        edglkp = Map.fromList [(1,[3]),(3,[4]),(5,[6]),(7,[8]),(9,[10])]
        
        f x lvl mem = (fromMaybe [] $ edglkp Map.!? x, x:mem)

        (res, mem) = dfsMem f (const.  (==) 9) seed []

        (fnd,atlvl) = fromMaybe (0,0) res
        
        pass = isNothing res && mem == [4,3,1]

        err = show (res,mem)
    in
        ("dfMemTest0", pass, err)

dfMemTest1 :: (String,Bool,String)
dfMemTest1 =
    let
        seed = [1]

        edglkp = Map.fromList [(1,[3]),(3,[5,4,7]),(5,[6]),(7,[8,9]),(9,[10])]
        
        f x lvl mem = (fromMaybe [] $ edglkp Map.!? x, x:mem)

        (res, mem) = dfsMem f (const.  (==) 9) seed []

        (fnd,atlvl) = fromMaybe (0,0) res
        
        pass = isJust res && fnd == 9 && atlvl == 3 && mem == [8,7,4,6,5,3,1]

        err = show (res,mem)
    in
        ("dfMemTest1", pass, err)

------------------------------------------------------------------------------------------
bfTest :: IO()
bfTest = 
    let
        res = foldl (flip (:)) [] [bfTest0,bfTest1, bfTest2]

    in
        printf $ unlines $ map (\(a,b,c) -> a ++ ": " ++ resultDetails b c) res

bfTest0 :: (String,Bool,String)
bfTest0 =
    let
        seed = [1]

        edglkp = Map.fromList [(1,[3]),(3,[9]),(5,[6]),(7,[8]),(9,[10])]
        
        f x lvl = fromMaybe [] $ edglkp Map.!? x

        (fnd,atlvl) = bfsTree f (== 9) seed

        pass = fnd == [9] && atlvl == 2

        err = show (fnd,atlvl)
    in
        ("bfTest0", pass, err)

bfTest1 :: (String,Bool,String)
bfTest1 =
    let
        seed = [1]

        edglkp = Map.fromList [(1,[3]),(3,[4]),(5,[6]),(7,[8]),(9,[10])]
        
        f x lvl = fromMaybe [] $ edglkp Map.!? x

        (fnd,atlvl) = bfsTree f (== 9) seed

        pass = fnd == []

        err = show (fnd,atlvl)
    in
        ("bfTest1", pass, err)

bfTest2 :: (String,Bool,String)
bfTest2 =
    let
        seed = [1,9]

        edglkp = Map.fromList [(1,[3]),(3,[4]),(5,[6]),(7,[8]),(9,[10])]
        
        f x lvl = fromMaybe [] $ edglkp Map.!? x

        (fnd,atlvl) = bfsTree f (== 10) seed

        pass = fnd ==[10] && atlvl == 1

        err = show (fnd,atlvl)
    in
        ("bfTest2", pass, err)
------------------------------------------------------------------------------------------
bfMemTest :: IO()
bfMemTest = 
    let
        res = foldl (flip (:)) [] [bfMemTest0,bfMemTest1] --, bfMemTest2]

    in
        printf $ unlines $ map (\(a,b,c) -> a ++ ": " ++ resultDetails b c) res

bfMemTest0 :: (String,Bool,String)
bfMemTest0 =
    let
        seed = [1]

        edglkp = Map.fromList [(1,[3]),(3,[4]),(5,[6]),(7,[8]),(9,[10])]
        
        f x lvl mem = (fromMaybe [] $ edglkp Map.!? x, x:mem)

        (fnd, atlvl, mem) = bfsMem f (const.  (==) 9) seed []

        pass = fnd == [] && mem == [4,3,1]

        err = show (fnd,atlvl,mem)
    in
        ("bfMemTest0", pass, err)

bfMemTest1 :: (String,Bool,String)
bfMemTest1 =
    let
        seed = [1]

        edglkp = Map.fromList [(1,[3]),(3,[5,4,7]),(5,[6]),(7,[8,9]),(9,[10])]
        
        f x lvl mem = (fromMaybe [] $ edglkp Map.!? x, x:mem)

        (fnd, atlvl, mem) = bfsMem f (const.  (==) 9) seed []

        pass = fnd == [9] && atlvl == 3 && mem == [9,8,6,7,4,5,3,1]

        err = show (fnd,atlvl,mem)
    in
        ("bfMemTest1", pass, err)

------------------------------------------------------------------------------------------
-- floydWarshall

floydWarshallTest :: IO()
floydWarshallTest = 
    let
        res = foldl (flip (:)) [] [floydWarshall0,floydWarshall1, floydWarshall2]

    in
        printf $ unlines $ map (\(a,b,c) -> a ++ ": " ++ resultDetails b c) res

floydWarshall0 :: (String,Bool,String)
floydWarshall0 =
    let
        seed = [1,2,3,4,5,6,7,8,9]

        edglkp = Map.fromList [(1,[(3,1),(4,3)]),(3,[(4,1)]),(4,[(1,1)]),(5,[(6,2)]),(7,[(8,2)]),(9,[(10,2)])]
        
        f x = fromMaybe [] $ edglkp Map.!? x
    
        map1 = floydWarshall f seed (+)

        pass = sort (Map.toList map1)  == [((1,3),1),((1,4),2),((3,4),1),((4,1),1),((5,6),2),((7,8),2),((9,10),2)]

        err = show map1
    in
        ("floydWarshall0", pass, err)

floydWarshall1 :: (String,Bool,String)
floydWarshall1 =
    let
        seed = [1,2,3,4,5,6,7,8,9]

        edglkp = Map.fromList [(1,[(3,1),(4,3)]),(3,[(4,0)]),(4,[(1,1)]),(5,[(6,2)]),(7,[(8,2)]),(9,[(10,2)])]
        
        f x = fromMaybe [] $ edglkp Map.!? x
    
        map1 = floydWarshall f seed (*)

        pass = sort (Map.toList map1)  == [((1,3),1),((1,4),0),((3,4),0),((4,1),1),((5,6),2),((7,8),2),((9,10),2)]

        err = show map1
    in
        ("floydWarshall1", pass, err)

floydWarshall2 :: (String,Bool,String)
floydWarshall2 =
    let
        seed = [1,2,3,4,5,6,7,8,9]

        edglkp = Map.fromList [(1,[(3,1),(4,3)]),(3,[(4,-1)]),(4,[(1,1)]),(5,[(6,2)]),(7,[(8,2)]),(9,[(10,2)])]
        
        f x = fromMaybe [] $ edglkp Map.!? x
    
        map1 = floydWarshall f seed (+)

        pass = sort (Map.toList map1)  == [((1,3),1),((1,4),0),((3,4),-1),((4,1),1),((5,6),2),((7,8),2),((9,10),2)]

        err = show map1
    in
        ("floydWarshall2", pass, err)
--
------------------------------------------------------------------------------------------
-- dijkstra

dijkstraTest :: IO()
dijkstraTest = 
    let
        res = foldl (flip (:)) [] [dijkstra0,dijkstra1, dijkstra2]

    in
        printf $ unlines $ map (\(a,b,c) -> a ++ ": " ++ resultDetails b c) res

dijkstra0 :: (String,Bool,String)
dijkstra0 =
    let
        seed = [(1,0)]

        edglkp = Map.fromList [(1,[(3,1),(4,3)]),(3,[(4,1)]),(4,[(1,1)]),(5,[(6,2)]),(7,[(8,2)]),(9,[(10,2)])]
        
        f x w = map (second (+w)) $ fromMaybe [] $ edglkp Map.!? x
        g (x,w) = x == 10
    
        map1 = dijkstraMultSrc f seed g

        pass = sort (Map.toList map1)  == ([(1,0),(3,1),(4,2)] ::[(Int,Int)])

        err = show map1
    in
        ("dijkstra0", pass, err)

dijkstra1 :: (String,Bool,String)
dijkstra1 =
    let
        seed = [(1,0)]

        edglkp = Map.fromList [(1,[(3,1),(4,3)]),(3,[(4,1)]),(4,[(5,1)]),(5,[(7,2)]),(7,[(8,2)]),(9,[(10,2)])]
        
        f x w = map (second (+w)) $ fromMaybe [] $ edglkp Map.!? x
        g (x,w) = x == 8
    
        map1 = dijkstraMultSrc f seed g

        pass = sort (Map.toList map1)  == ([(1,0),(3,1),(4,2),(5,3),(7,5),(8,7)] ::[(Int,Int)])

        err = show map1
    in
        ("dijkstra1", pass, err)

dijkstra2 :: (String,Bool,String)
dijkstra2 =
    let
        seed = [(1,0)]

        edglkp = Map.fromList [(1,[(3,1),(4,3)]),(3,[(4,1)]),(4,[(5,1)]),(5,[(7,2)]),(7,[(8,2)]),(9,[(10,2)])]
        
        f x w = map (second (+w)) $ fromMaybe [] $ edglkp Map.!? x
        g (x,w) = x == 7
    
        map1 = dijkstraMultSrc f seed g

        pass = sort (Map.toList map1)  == ([(1,0),(3,1),(4,2),(5,3),(7,5)] ::[(Int,Int)])

        err = show map1
    in
        ("dijkstra1", pass, err)

--
------------------------------------------------------------------------------------------
-- aStar

aStarTest :: IO()
aStarTest = 
    let
        res = foldl (flip (:)) [] [aStar0,aStar1, aStar2, aStar3,aStar3']

    in
        printf $ unlines $ map (\(a,b,c) -> a ++ ": " ++ resultDetails b c) res

aStar0 :: (String,Bool,String)
aStar0 =
    let
        seed = [(1,0)]

        edglkp = Map.fromList [(1,[(3,1),(4,3)]),(3,[(4,1)]),(4,[(1,1)]),(5,[(6,2)]),(7,[(8,2)]),(9,[(10,2)])]
        
        f x w = map (second (+w)) $ fromMaybe [] $ edglkp Map.!? x
        g (x,w) = x == 10
    
        map1 = aStarMultSrc f (const 0) seed g

        pass = sort (Map.toList map1)  == ([(1,0),(3,1),(4,2)] ::[(Int,Int)])

        err = show map1
    in
        ("aStar0", pass, err)

aStar1 :: (String,Bool,String)
aStar1 =
    let
        seed = [(1,0)]

        edglkp = Map.fromList [(1,[(3,1),(4,3)]),(3,[(4,1)]),(4,[(5,1)]),(5,[(7,2)]),(7,[(8,2)]),(9,[(10,2)])]
        
        f x w = map (second (+w)) $ fromMaybe [] $ edglkp Map.!? x
        g (x,w) = x == 8
    
        map1 = aStarMultSrc f (const 0) seed g

        pass = sort (Map.toList map1)  == ([(1,0),(3,1),(4,2),(5,3),(7,5),(8,7)] ::[(Int,Int)])

        err = show map1
    in
        ("aStar1", pass, err)

aStar2 :: (String,Bool,String)
aStar2 =
    let
        seed = [(1,0)]

        edglkp = Map.fromList [(1,[(3,1),(4,3)]),(3,[(4,1)]),(4,[(5,1)]),(5,[(7,2)]),(7,[(8,2)]),(9,[(10,2)])]
        
        f x w = map (second (+w)) $ fromMaybe [] $ edglkp Map.!? x
        g (x,w) = x == 7
    
        map1 = aStarMultSrc f (const 0) seed g

        pass = sort (Map.toList map1)  == ([(1,0),(3,1),(4,2),(5,3),(7,5)] ::[(Int,Int)])

        err = show map1
    in
        ("aStar2", pass, err)

aStar3 :: (String,Bool,String)
aStar3 =
    let
        seed = [(1,0)]

        edglkp = Map.fromList [(1,[(3,100),(4,20)]),(4,[(5,20)]),(5,[(6,20)]),(6,[(7,20)]),(7,[(8,20)]),(8,[(9,20)])]
        
        f x w = map (second (+w)) $ fromMaybe [] $ edglkp Map.!? x
        g (x,w) = x == 3

        map1 = aStarMultSrc f (const 0) seed g

        pass = sort (Map.toList map1)  == ([(1,0),(3,100),(4,20),(5,40),(6,60),(7,80)] ::[(Int,Int)])

        err = show map1
    in
        ("aStar3", pass, err)

aStar3' :: (String,Bool,String)
aStar3' =
    let
        seed = [(1,0)]

        edglkp = Map.fromList [(1,[(3,100),(4,20)]),(4,[(5,20)]),(5,[(6,20)]),(6,[(7,20)]),(7,[(8,20)]),(8,[(9,20)])]
        
        f x w = map (second (+w)) $ fromMaybe [] $ edglkp Map.!? x
        g (x,w) = x == 3
        
        h x
            | x == 3 = 0
            | otherwise = 100

        map1 = aStarMultSrc f h seed g

        pass = sort (Map.toList map1)  == ([(1,0),(3,100)] ::[(Int,Int)])

        err = show map1
    in
        ("aStar3'", pass, err)
