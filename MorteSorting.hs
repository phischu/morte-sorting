{-# LANGUAGE RankNTypes,NoImplicitPrelude,ScopedTypeVariables #-}
module MorteSorting where

import Prelude (Int,(+),undefined)

const :: a -> b -> a
const x _ = x

type Bool = forall x . x -> x -> x

true :: Bool
true x _ = x

false :: Bool
false _ y = y

bool :: Bool -> x -> x -> x
bool b = b

type List a = forall x . (a -> x -> x) -> x -> x

empty :: List a
empty _ nil = nil

prepend :: forall x a . a -> ((a -> x -> x) -> x -> x) -> ((a -> x -> x) -> x -> x)
prepend x l cons nil = cons x (l cons nil)

foldr :: forall x a . ((a -> x -> x) -> x -> x) -> (a -> x -> x) -> x -> x
foldr l = l

type Pair a b = forall x . (a -> b -> x) -> x

pair :: a -> b -> Pair a b
pair x y runPair = runPair x y

fst :: Pair a b -> a
fst runPair = runPair (\x _ -> x)

snd :: Pair a b -> b
snd runPair = runPair (\_ y -> y)

append :: List a -> List a -> List a
append l1 l2 = foldr l1 prepend (foldr l2 prepend empty)

sort :: (a -> a -> Bool) -> List a -> List a
sort compare l = foldr l (insert compare) empty

insert :: forall x a . (a -> a -> Bool) -> a -> ((a -> x -> x) -> x -> x) -> ((a -> x -> x) -> x -> x)
insert compare x l cons nil = case head l of
    Nothing -> cons x nil
    Just y -> case compare x y of
        True -> cons x (insert compare y rest cons nil)
        False -> cons y (insert compare x rest cons nil)

example :: List Int
example cons nil = cons 1 (cons 2 (cons 3 nil))

map :: (a -> b) -> List a -> List b
map f l cons = l (\a x -> cons (f a) x)


result :: Int
result = foldr (map (+ 1) example) (+) 0




