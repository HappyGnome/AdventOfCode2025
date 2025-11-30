{-

Copyright HappyGnome 2025 (https://github.com/HappyGnome)

DISCLAIMER: 
Please don't copy this code, I'm new to Haskell and have probably made many mistakes
of style and logic. This is a learning project for the author.

-}

module Shorts (
    rpad
    ,lpad
    ,maxLineLen 
    ,lJustify 
    ,rJustify 
    ,gridDims 
    ,splitOnPred 
    ,foldl2x1
    ,foldl2x2
    ,pairMap1
    ,pairMap2
    ,diagsUL
    ,makeAllPairs
    ,mod1
    ,rowSwap
    ,colSwap
    ,rowShift
    ,colShift
    ,rowLin
    ,colLin
    ,rowDel
    ,colDel
    ,rowIns
    ,colIns
    ,cycleList
    ,rowCycle
    ,colCycle
    ,rowRepl
    ,colRepl
    ,colGet
    ,applyRowColOp
    ,RowColOp(
        ROWSWAP 
        ,COLSWAP 
        ,ROWSCALE
        ,COLSCALE
        ,ROWSHIFT
        ,COLSHIFT
        ,ROWLIN 
        ,COLLIN 
        ,ROWADD
        ,COLADD
        ,ALLADD
        ,ALLSCALE
        ,ROWDEL 
        ,COLDEL 
        ,ROWCYCLE  
        ,COLCYCLE  
        ,ROWINS  
        ,COLINS  
        ,ROWREPL  
        ,COLREPL  
        ,NORCOP)
    ,newRNGFromSeed 
    ,newRNG 
    ,permuteRand
) where

--import Data.Function
import Data.List (sortBy, sortOn)
--import Data.List.Extra (groupOnKey)
import Data.Word
import Data.Time.Clock.System
import System.Random
import Debug.Trace

----------------------------------------------------- 
-- Grid helpers

-- | rpad n a xs
-- append 'a' to 'xs' until the length is 'n'
-- if 'xs' is longer than 'n', remove elements from the end (right) until it is
rpad::Int -> a -> [a] -> [a]
rpad n a xs
    | l == n = xs
    | l > n = take n xs
    | otherwise = xs ++ replicate (n - l) a 
    where l = length xs

-- | lpad n a xs
-- prepend 'a' to 'xs' until the length is 'n'
-- if 'xs' is longer than 'n', remove elements from the beginning (left) until it is
lpad::Int -> a -> [a] -> [a]
lpad n a xs
    | l == n = xs
    | l > n = drop (l - n) xs
    | otherwise = replicate (n - l) a ++ xs 
    where l = length xs

-- | Get the length of the longest row of a 2D array
maxLineLen::[[a]] -> Int
maxLineLen [] = 0
maxLineLen lss = maximum $ map length lss

-- | Pad each row on the right with a given value so that all rows have same length as the longest row of the input
lJustify:: a -> [[a]] -> [[a]]
lJustify a xxs =
    let 
        mx = maxLineLen xxs
    in
        map (rpad mx a) xxs


-- | Pad each row on the left with a given value so that all rows have same length as the longest row of the input
rJustify:: a -> [[a]] -> [[a]]
rJustify a xxs =
    let 
        mx = maxLineLen xxs
    in
        map (lpad mx a) xxs

-- | Get (Row count, max column count)
gridDims :: [[b]] -> (Int,Int)
gridDims lss = (length lss, maxLineLen lss)

-----------------------------------------------------

-- | splitOnPred f css
-- Get the list of substrings of 'css' between elements satisfying predicate 'f' (and excluding those elements)
splitOnPred :: (a->Bool) -> [a] -> [[a]]
splitOnPred _ [] = []
splitOnPred f css = 
    let
        g (acc0, cs) c
            | f c = (acc0 ++ [cs], [])
            | otherwise = (acc0, cs ++ [c])

        fin (acc0, cs) = acc0 ++ [cs]
    in
        fin $ foldl g ([],[]) css 
------------------------------------------------------

-- | left fold pairs from a list, with stride 1
-- i.e. "foldl2x1 f 0 [1,2..]" calls "f 0 1 2" then "f (f 0 1 2) 2 3" ...
foldl2x1:: (b -> a -> a -> b) -> b -> [a] -> b
foldl2x1 _ acc [] = acc
foldl2x1 _ acc [_] = acc
foldl2x1 f acc (x:y:xs) = foldl2x1 f (f acc x y) (y:xs)

-- | left fold pairs from a list, with stride 2
-- i.e. "foldl2x2 f 0 [1,2..]" calls "f 0 1 2" then "f (f 0 1 2) 3 4" ...
foldl2x2 :: (b -> a -> a -> b) -> b -> [a] -> b
foldl2x2 _ acc [] = acc
foldl2x2 _ acc [_] = acc
foldl2x2 f acc (x:y:xs) = foldl2x2 f (f acc x y) xs
--

-- | map a list in pairs with stride 1
pairMap1 :: (a -> a -> b) -> [a] -> [b]
pairMap1 f =  foldl2x1 (\ acc x y -> acc ++ [f x y]) []

-- | map a list in pairs with stride 2
pairMap2 :: (a -> a -> b) -> [a] -> [b]
pairMap2 f =  foldl2x2 (\ acc x y -> acc ++ [f x y]) []

-------------------------------------------------------
--

diagsUL:: [[a]] -> [[a]]
diagsUL =
    let
        empt = [] : empt

        f (op,cl) ls = 
            (zipWith (:) ls (acc0 ++ empt), acc1 ++ cl)
            where
                (acc0,acc1) = splitAt (length ls) ([] : op)
    in
        uncurry (++).foldl f ([],[]) 

-------------------------------------------------------

-- | Make all pairs of lines with elements from a list in the order they appear in the given list
makeAllPairs :: [a] -> [(a,a)]
makeAllPairs [] = []
makeAllPairs [_] = []
makeAllPairs (x:xs) = 
    map (\ y -> (x,y)) xs ++ makeAllPairs xs

-------------------------------------------------------
-- Parity / sanity helpers

-- | Modulo base 1 i.e. "a mod1 b" is between 1 and "b" inclusive
mod1 :: (Integral i) => i -> i -> i
mod1 x by = 1 + ( (x - 1) `mod` by)

-------------------------------------------------------
-- Row / Col ops

-- | Swap rows i and j in a given list
-- (Not recommended for use in most algorithms - just for reshaping arrays/matrices)
rowSwap :: Int -> Int -> [a]-> [a]
rowSwap i j ls
    | i == j = ls
    | otherwise =
        let 
            i' = min i j
            j' = max i j
            
            (st,nd0) = splitAt i' ls
            (mid,nd) = splitAt (j' - i') nd0
        in
        concat [st,[head nd],tail mid,[head mid],tail nd]

-- | Swap columns i and j in a 2D array
colSwap :: Int -> Int -> [[a]] -> [[a]]
colSwap i j = map (rowSwap i j)        
 
-- | Cycle values in a list by a number of positions 
-- E.g. cycleList 1 [1,2,3] == [3,1,2]
-- E.g. cycleList -1 [1,2,3] == [2,3,1]
cycleList :: Int -> [a] -> [a]
cycleList i ls
    | i == 0 = ls
    | otherwise =
        let 
            j = (- i ) `mod` length ls


            (st,nd) = splitAt j ls
        in
            nd ++ st

-- | Delete element at index i from a list
-- (Not recommended for use in most algorithms - just for reshaping arrays/matrices)
rowDel :: Int -> [a] -> [a]
rowDel i ls =
    let
        (st,nd) = splitAt i ls
    in
        st ++ tail nd

-- | Delete column i from a 2D array
colDel :: Int -> [[a]] -> [[a]]
colDel i = map (rowDel i)

-- | Extract a column from a 2D array
-- (Not recommended for use in most algorithms - just for reshaping arrays/matrices)
colGet :: Int -> [[a]] -> [a]
colGet i = map (!! i)

-- | Insert a row into a list at a fixed position
-- (Not recommended for use in most algorithms - just for reshaping arrays/matrices)
rowIns :: Int -> a -> [a] -> [a]
rowIns i x ls = 
    let
        (st,nd) = splitAt i ls
    in
        concat[st, [x], nd]

-- | Insert column into a 2D array
colIns :: Int -> [a] -> [[a]] -> [[a]]
colIns i = 
    zipWith (rowIns i)

-- | Replace a row (item in a list)
-- (Not recommended for use in most algorithms - just for reshaping arrays/matrices)
rowRepl :: Int -> a -> [a] -> [a]
rowRepl i x ls = 
    let
        (st,nd) = splitAt i ls
    in
        concat[st, [x], tail nd]

-- | Replace a column in a 2D array
colRepl :: Int -> [a] -> [[a]] -> [[a]]
colRepl i =
    zipWith (rowRepl i)

-- | Cycle items in a single row by a set distance (positive for a cycle rightwards)
rowCycle :: Int -> Int -> [[a]] -> [[a]]
rowCycle by i lss =
    rowRepl i (cycleList by (lss !! i)) lss

-- | Cycle items in a single column by a set distance (Positive for a cycle downwards)
colCycle :: Int -> Int -> [[a]] -> [[a]]
colCycle by i lss =
    colRepl i (cycleList by (colGet i lss)) lss

-- | Move a row down or up by a set number of rows
-- (Not recommended for use in most algorithms - just for reshaping arrays/matrices)
rowShift :: Int -> Int -> [a] -> [a]
rowShift i j ls
    | i == j = ls
    | i < j =
        let 
            (st,nd0) = splitAt i ls
            (mid,nd) = splitAt (j - i + 1) nd0
        in
            concat[st,tail mid,[head mid], nd]
    | otherwise = -- i > j 
        let 
            (st,nd0) = splitAt j ls
            (mid,nd) = splitAt (i - j) nd0
        in
            concat[st, [head nd], mid, tail nd]

-- | Move a column left or right in a 2D array 
colShift :: Int -> Int -> [[a]] -> [[a]]
colShift i j = map (rowShift i j)     

-- | Apply binary operation elementwise to two rows, and store the result in the second row
-- (Not recommended for use in most algorithms - just for reshaping arrays/matrices)
rowLin :: (a -> a -> a) -> Int -> Int -> [a] -> [a]
rowLin f i j ls
    | i == j = 
        let 
            (st,nd) = splitAt i ls
            h = head nd
        in
            concat[st, [f h h], tail nd]
    | i < j =
        let 
            (st,nd0) = splitAt i ls
            (mid,nd) = splitAt (j - i) nd0
            hto = head nd
            hfrom = head mid

        in
            concat[st, mid, [ f hfrom hto], tail nd]
    | otherwise = -- i > j 
        let 
            (st,nd0) = splitAt j ls
            (mid,nd) = splitAt (i - j) nd0
            hto = head mid
            hfrom = head nd

        in
            concat[st, [ f hfrom hto ], tail mid, nd]

-- | Apply binary operation elementwise to two columns, and store the result in the second column
colLin :: (a -> a -> a) -> Int -> Int -> [[a]] -> [[a]]
colLin f i j = map (rowLin f i j)

-- | Enumerated list of reshaping ops for a 2D array
data RowColOp a = 
            ROWSWAP Int Int
         |  COLSWAP Int Int
         |  ROWSCALE a Int
         |  COLSCALE a Int
         |  ROWSHIFT Int Int
         |  COLSHIFT Int Int
         |  ROWLIN a Int Int
         |  COLLIN a Int Int
         |  ROWADD a Int
         |  COLADD a Int
         |  ALLSCALE a
         |  ALLADD a
         |  ROWDEL Int
         |  COLDEL Int
         |  ROWCYCLE Int Int
         |  COLCYCLE Int Int
         |  ROWINS [a] Int
         |  COLINS [a] Int
         |  ROWREPL [a] Int
         |  COLREPL [a] Int
         |  NORCOP

         deriving (Show)

applyRowColOp :: (Num a) => RowColOp a -> [[a]] -> [[a]]
applyRowColOp (ROWSWAP i j) = rowSwap i j
applyRowColOp (ROWSCALE x i) = rowLin (zipWith $ const.(* x) ) i i
applyRowColOp (ROWSHIFT i j) = rowShift i j
applyRowColOp (ROWLIN x i j) = rowLin (zipWith $ \ a b -> (a * x) + b) i j
applyRowColOp (ROWADD x i) = rowLin (zipWith $ const.(+ x)) i i

applyRowColOp (COLSWAP i j) = colSwap i j
applyRowColOp (COLSCALE x i) = colLin ( const.(* x) ) i i
applyRowColOp (COLSHIFT i j) = colShift i j
applyRowColOp (COLLIN x i j) = colLin (\ a b -> (a * x) + b) i j
applyRowColOp (COLADD x i) = colLin (const.(+ x)) i i

applyRowColOp (ALLADD x) = map (map (+x))
applyRowColOp (ALLSCALE x) = map (map (*x))


applyRowColOp (ROWDEL i) = rowDel i
applyRowColOp (COLDEL i) = colDel i
applyRowColOp (ROWCYCLE by i) = rowCycle by i
applyRowColOp (COLCYCLE by i) = colCycle by i
applyRowColOp (ROWINS ls i) = rowIns i ls
applyRowColOp (COLINS ls i) = colIns i ls
applyRowColOp (ROWREPL ls i) = rowRepl i ls
applyRowColOp (COLREPL ls i) = colRepl i ls

applyRowColOp NORCOP = id


-------------------------------------------------------
-- Random number generation (RNG)

-- | Create new RNG from a given seed
newRNGFromSeed ::  Int -> StdGen
newRNGFromSeed = mkStdGen
    

-- | Create new RNG using system time as the seed
newRNG :: IO StdGen
newRNG = do
    newRNGFromSeed . fromIntegral . systemNanoseconds <$> getSystemTime

-- | Create random permutation of a given list
permuteRand :: (RandomGen g) => g -> [a] -> (g,[a])
permuteRand gen ls = 
    let 
        genOne (g,acc) _ = (g', x: acc)
            where
                (x, g') = genWord32 g

        (gen',ord) = foldl genOne (gen,[]) ls
    in 
       trace (show ord) $ ( gen', map snd $ sortOn fst $ zip ord ls)

