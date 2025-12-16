{-

Copyright HappyGnome 2025 (https://github.com/HappyGnome)

DISCLAIMER: 
Please don't copy this code, I'm new to Haskell and have probably made many mistakes
of style and logic. This is a learning project for the author.

-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE FlexibleInstances #-}

module Memoize 
    (
        Memoized ()
        ,Memoized2()
        ,toMemoized
        ,toMemoized2
--        ,Factory
--        ,facRun
--        ,mapFactoryr
--        ,GenFactory(GenFactory)
--        --,facComp
--        ,facCompIn
--        ,facCompOut
--        ,facBranchIn
--        ,facBranchOut
    )
where

import qualified Data.Map as Map
import Data.Tuple
import Data.Maybe

--import MapFoldable


--class Factory s a b where
--    facRun :: s -> a -> (s,b)
--
--mapFactoryr :: (MapFoldable t b, Factory sfl a b) => sfl -> t a -> (sfl, t b) 
--mapFactoryr m fs = 
--    let
--        fld x n = (y ,m')
--            where
--                (m',y) = facRun n x
--    in
--        swap $ mapFoldr fld m fs

--TODO fold Factory
--

-- mapFactoryrWith :: (MapFoldable t b', Ord a) => (a' -> (a,d)) -> (d -> b -> b') -> Memoized a b -> t a' -> ( t b', Memoized a b) 
-- mapFactoryrWith etr pack m fs = 
--     let
--         fld x n = (pack ctx $ memoValue' m',m')
--             where
--                 (y,ctx) = etr x
--                 m' = memoExec n y
--     in
--         mapFold fld m fs
--                 
-- mapFactory2 :: (MapFoldable t c, Ord a, Ord b) => Memoized2 a b c -> t (a,b) -> ( t c, Memoized2 a b c) 
-- mapFactory2 m fs = 
--     let
--         fld (x,y) n = (memoValue' m',m')
--             where
--                 m' = memoExec2 n x y
--     in
--         mapFold fld m fs
-- 
-- mapFactoryWith2 :: (MapFoldable t c', Ord a, Ord b) => (a' -> (a,b,d)) -> (d -> c -> c') -> Memoized2 a b c -> t a' -> ( t c', Memoized2 a b c) 
-- mapFactoryWith2 m fs = 
--     let
--         fld (x,y) n = (memoValue' m',m')
--             where
--                 m' = memoExec2 n x y
--     in
--         mapFold fld m fs

----------------------------------------
-- TODO: Use a Monad or applicative?
--
-- Mf (a -> b) -> Mf a -> Mf b, Mf Mf b = Mf b where memoized values are unioned. Mf would have an apply f  function too


-- c -> (M f) c |  x :-> (M f Map.empty x) 
-- (c->d) -> Mf c -> Mf d | g (M f mp x) :-> (M f mp $ g x)
-- applyMemoized :: (M (a->b) (Map a b) a) -> (M (a->b) (Map a b) b) = ...
--
--

class SingleFunction s where
    eval :: s a b -> a -> b

-- Create instance of SingletonFunction for each op to memoize

data (Ord a, SingleFunction f a b) => Memoized f c = Memoized (a -> b) (Map.Map a b) c

data (Ord a, Ord b) => Memoized2 a b c d = Memoized2 (a -> b  -> c) (Map.Map (a,b) c) d

instance (Ord a, SingleFunction f a b) => Monad (Memoized f) where
    
    (>>=) (Memoized f mp x) g = 
        let
            Memoized f' mp' y = g x
        in
            -- f is a SingleFunction, so f = f'
            Memoized f (Map.union mp mp') 


    toFacFuncR (Memoized f mp) x 
        | Just r <- res = (Memoized f mp, r)
        | otherwise = (Memoized f mp', res')
        where
            res = mp Map.!? x
            res' = f x
            mp' = Map.insert x res' mp

instance (Ord a, Ord b) => FacPlumbable (Memoized2 a b c) (a,b) c where

    facRun (Memoized2 f mp) xy
        | Just r <- res = (Memoized2 f mp, r)
        | otherwise = (Memoized2 f mp', res')
        where
            res = mp Map.!? xy
            res' = uncurry f xy
            mp' = Map.insert xy res' mp

toMemoized :: (Ord a) => (a -> b) -> Memoized a b
toMemoized f = Memoized f Map.empty 

toMemoized2 :: (Ord a, Ord b) => (a -> b -> c) -> Memoized2 a b c
toMemoized2 f = Memoized2 f Map.empty

------------------------------------------------------------------------------------------
-- GenFactory
-- Generic factory can be built by composing other factories and functions


--data GenFactory t a c = GenFactory (t -> a -> (t,c)) t
--
--instance Factory (GenFactory t a b) a b where
--    facRun (GenFactory f t) x = (g,y)
--        where
--            (t', y) = f t x
--            g = GenFactory f t'
--
---- TODO
---- facComp :: (Factory s b c, Factory t a b) => s -> t -> GenFactory (s,t) a c 
---- facComp f g =
----     let
----         f' (f0,g0) x = ((f0',g0'),z)
----             where
----                 (g0',y)  = facRun g0 x
----                 (f0',z)  = facRun f0 y
----     in
----         GenFactory f' (f,g)
--
--facCompIn :: (Factory s b c) => s -> (a -> b) -> GenFactory s a c 
--facCompIn f g =
--    let
--        newF f0 x = (f0', z)
--            where
--                y = g x
--                (f0',z)  = facRun f0 y
--    in
--        GenFactory newF f
--
--facBranchIn :: (Factory s b c) => s -> (a -> (b,d)) -> GenFactory s a (c,d) 
--facBranchIn f g =
--    let
--        newF f0 x = (f0', (z,byp))
--            where
--                (y,byp) = g x
--                (f0',z)  = facRun f0 y
--    in
--        GenFactory newF f
--
--
--facCompOut :: (Factory s a b) => (b -> c) -> s -> GenFactory s a c 
--facCompOut f g =
--    let
--        newF g0 x = (g0', f y)
--            where
--                (g0',y)  = facRun g0 x
--    in
--        GenFactory newF g
--
--facBranchOut :: (Factory s a b) => (b -> c -> d) -> s -> GenFactory s (a,c) d 
--facBranchOut f g =
--    let
--        newF g0 (x,byp) = (g0', f y byp)
--            where
--                (g0',y)  = facRun g0 x
--    in
--        GenFactory newF g
