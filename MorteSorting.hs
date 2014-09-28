{-# LANGUAGE RankNTypes,NoImplicitPrelude,ExistentialQuantification,DeriveFunctor #-}
module MorteSorting where

import Data.Functor (Functor,fmap)
import Data.Function((.))
import Prelude (Ord,(<=),undefined,id)

fromList :: [a] -> List a
fromList [] = wrap Empty
fromList (a:as) = wrap (Cons a (fromList as))

toList :: Stream a -> [a]
toList stream = case unwrap stream of
    Empty -> []
    Cons a stream' -> a : toList stream'


data Mu f = Mu (forall x . (f x -> x) -> x)

wrap :: (Functor f) => f (Mu f) -> Mu f
wrap layer = Mu (\alg -> alg (fmap (fold alg) layer))

fold :: (f x -> x) -> Mu f -> x
fold alg (Mu mu) = mu alg

data Nu g = forall s . Nu (s -> g s) s

unfold :: (s -> g s) -> s -> Nu g
unfold coalg s = Nu coalg s

unwrap :: (Functor g) => Nu g -> g (Nu g)
unwrap (Nu coalg s) = fmap (unfold coalg) (coalg s)

data Fix f = In {out :: f (Fix f)}


muToNu :: (Functor f) => Mu f -> Nu f
muToNu = fold (unfold (fmap unwrap))


data L a x = Empty | Cons a x deriving (Functor)

type List a = Mu (L a)

type Stream a = Nu (L a)



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

