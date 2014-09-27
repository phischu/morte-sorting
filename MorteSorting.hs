{-# LANGUAGE RankNTypes,NoImplicitPrelude,ScopedTypeVariables,ExistentialQuantification,DeriveFunctor #-}
module MorteSorting where

import Data.Functor (Functor,fmap)
import Data.Function((.))
import Prelude (Ord,(<=),undefined)

fromList :: [a] -> List a
fromList [] = Mu (\f -> f Empty)
fromList (a:as) = wrap (Cons a (fromList as))

toList :: Stream a -> [a]
toList stream = case unwrap stream of
    Empty -> []
    Cons a stream' -> a : toList stream'

data Mu f = Mu {unMu :: forall x . (f x -> x) -> x}

data Nu f = forall x .  Nu ((x -> f x,x))

data L a x = Empty | Cons a x deriving (Functor)

type List a = Mu (L a)

type Stream a = Nu (L a)

fold :: (f x -> x) -> Mu f -> x
fold f mu = unMu mu f

unfold :: (x -> f x) -> x -> Nu f
unfold f x = Nu (f,x)

wrap :: (Functor f) => f (Mu f) -> Mu f
wrap fmu = Mu (\f -> f (fmap (fold f) fmu))

unwrap :: (Functor f) => Nu f -> f (Nu f)
unwrap (Nu (f,s)) = fmap (unfold f) (f s)

insertionSort :: (Ord a) => List a -> Stream a
insertionSort = fold insert

insert :: (Ord a) => L a (Stream a) -> Stream a
insert = unfold (swap . fmap unwrap)

selectionSort :: (Ord a) => List a -> Stream a
selectionSort = unfold select

select :: (Ord a) => List a -> L a (List a)
select = fold (fmap wrap . swap)

swap :: (Ord a) => L a (L a x) -> L a (L a x)
swap Empty = Empty
swap (Cons a Empty) = Cons a Empty
swap (Cons a1 (Cons a2 x)) = if a1 <= a2 then Cons a1 (Cons a2 x) else (Cons a2 (Cons a1 x))

