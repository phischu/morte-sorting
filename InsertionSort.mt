-- let
--    wrap = 

(  \(a : *)
->
(  \(L : * -> * -> *)
-> \(Mu : (* -> *) -> * -> *)
-> \(fold : forall (f:* -> *) -> forall (x:*) -> (f x -> x) -> Mu f x -> x)
-> \(wrap : forall (f:* -> *) -> forall (x:*) -> (f (Mu f x)) -> Mu f x)
-> \(x:*)
-> fold (L a) (Mu (L a) x) (wrap (L a) x))

-- data L a x = Empty | Cons a x
(\(a : *) -> \(x : *) -> forall (y : *) -> y -> (a -> x -> y) -> y)

-- type Mu f x = (f x -> x) -> x
(\(f:* -> *) -> \(x:*) -> (f x -> x) -> x)

-- fold alg (Mu mu) = mu alg
(\(f:* -> *) -> \(x:*) -> \(alg : f x -> x) -> \(mu : (f x -> x) -> x) -> mu alg)

-- wrap layer = Mu (\alg -> alg (fmap (fold alg) layer))
(\(f:* -> *) -> \(x:*) -> \(layer : f ((f x -> x) -> x)) -> \(alg : f x -> x) -> alg (fmap (fold alg) layer))

)


