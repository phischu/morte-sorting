{-# LANGUAGE ExistentialQuantification #-}
module HaskellSorting where

import Prelude hiding (foldr)

data ListF a x = Empty | Cons a x

type Fold f x = (f x -> x) -> x

fold :: Fold f x -> (f x -> x) -> x
fold = id

inn :: (Functor f) => f (Fold f x) -> Fold f x
inn f u = u (fmap (\fuu -> fuu u) f)

unInn :: (Functor f) => Fold f x -> f (Fold f x)
unInn = undefined

type Unfold f x = (x -> f x,x)

out :: (Functor f) => f (Unfold f x) -> Unfold f x
out = undefined

unOut :: (Functor f) => Unfold f x -> f (Unfold f x)
unOut (step,x) = fmap (\x' -> (step,x')) (step x)

{-
data List a = Empty | Cons a (List a) deriving Show

data Stream a = forall s . Stream s (s -> Maybe (a,s))

listToStream :: List a -> Stream a
listToStream l = unfold l (\_ -> undefined)

unfold :: s -> (s -> Maybe (a, s)) -> Stream a
unfold = Stream

next :: Stream a -> Maybe (a,Stream a)
next (Stream s f) = case f s of
    Nothing -> Nothing
    Just (a,s') -> Just (a,Stream s' f)

listCase :: List a -> Maybe (a,List a)
listCase = undefined

streamToList :: Stream a -> List a
streamToList s = case next s of
    Nothing -> Empty
    Just (x,s') -> cons x (streamToList s')

empty :: List a
empty = Empty

cons :: a -> List a -> List a
cons = Cons

foldr :: List a -> (a -> x -> x) -> x -> x
foldr Empty _ nil = nil
foldr (Cons x xs) f nil = f x (foldr xs f nil)

insert :: (Ord a) => a -> List a -> List a
insert x Empty = cons x Empty
insert x (Cons y ys) = case compare x y of
    LT -> Cons x (insert y ys)
    EQ -> Cons x (insert y ys)
    GT -> Cons y (insert x ys)

sort :: (Ord a) => List a -> List a
sort l = foldr l insert empty
-}