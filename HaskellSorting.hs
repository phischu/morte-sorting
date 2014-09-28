{-# LANGUAGE RankNTypes,NoImplicitPrelude,ExistentialQuantification,DeriveFunctor #-}
module MorteSorting where

import Data.Functor (Functor,fmap)
import Data.Function((.))
import Prelude (Ord,(<=),undefined)

fromList :: [a] -> List a
fromList [] = wrap Empty
fromList (a:as) = wrap (Cons a (fromList as))

toList :: Stream a -> [a]
toList stream = case unwrap stream of
    Empty -> []
    Cons a stream' -> a : toList stream'

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
swap (Cons a1 (Cons a2 x)) =
    if a1 <= a2
    then Cons a1 (Cons a2 x)
    else Cons a2 (Cons a1 x)

data L a x = Empty | Cons a x deriving (Functor)

data Fix f = In {out :: f (Fix f)}

type List a = Fix (L a)

type Stream a = Fix (L a)

fold :: (Functor f) => (f x -> x) -> Fix f -> x
fold alg = alg . fmap (fold alg) . out

wrap :: f (Fix f) -> Fix f
wrap = In

unfold :: (Functor g) => (s -> g s) -> s -> Fix g
unfold coalg = In . fmap (unfold coalg) . coalg

unwrap :: Fix g -> g (Fix g)
unwrap = out




