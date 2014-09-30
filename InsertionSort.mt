-- Foreign imports
(  \(a : *)
->

-- Module definitions
(  \(L_a : * -> *)
-> \(Mu : (* -> *) -> *)
-> \(Nu : (* -> *) -> *)
-> \(fmapL : forall (x:*) -> forall (y:*) -> (x -> y) -> L_a x -> L_a y)
-> \(fold : forall (f : * -> *) -> forall (x:*) -> (f x -> x) -> Mu f -> x)
-> \(wrap : forall (f : * -> *) -> (forall (x:*) -> forall (y:*) -> (x -> y) -> f x -> f y) -> (forall (f : * -> *) -> forall (x:*) -> (f x -> x) -> Mu f -> x) -> f (Mu f) -> Mu f)
-> \(unfold : forall (f: * -> *) -> forall (s:*) -> (s -> f s) -> s -> Nu f)
-> \(unwrap : Nu L_a -> L_a (Nu L_a))
-> \(swap : forall (x:*) -> L_a (L_a x) -> L_a (L_a x))
->  fold L_a (Nu L_a) (unfold L_a (L_a (Nu L_a)) (\(s : L_a (Nu L_a)) ->
        swap (Nu L_a) (fmapL (Nu L_a) (L_a (Nu L_a)) unwrap s)))
)

-- L_a
(\(x:*) -> forall (z:*) -> z -> (a -> x -> z) -> z)

-- Mu
(\(f : * -> *) -> forall (x:*) -> (f x -> x) -> x)

-- Nu
(\(f : * -> *) -> forall (y:*) -> (forall (s:*) -> (s -> f s) -> s -> y) -> y)

-- fmapL
(\(x:*) -> \(y:*) -> \(f : x -> y) -> \(lax : forall (z:*) -> z -> (a -> x -> z) -> z) ->
    \(z:*) -> \(empty : z) -> \(cons : a -> y -> z) ->
        lax z empty (\(va:a) -> \(vx:x) -> cons va (f vx)) )

-- fold
(\(f : * -> *) -> \(x:*) -> \(alg : f x -> x) -> \(mu_f : forall (x:*) -> (f x -> x) -> x) -> mu_f x alg)

-- wrap
(   \(f : * -> *)
->  \(fmap : forall (x:*) -> forall (y:*) -> (x -> y) -> f x -> f y)
->  \(fold : forall (f : * -> *) -> forall (x:*) -> (f x -> x) -> (forall (x:*) -> (f x -> x) -> x) -> x)
->  \(f_mu_f : f (forall (x:*) -> (f x -> x) -> x))
->  \(y:*) -> \(alg : f y -> y) -> alg (fmap (forall (x:*) -> (f x -> x) -> x) y (fold f y alg) f_mu_f))

-- unfold
(   \(f : * -> *)
->  \(s : *)
->  \(coalg : s -> f s)
->  \(vs : s)
->  \(y : *)
->  \(caseNu : forall (s:*) -> (s -> f s) -> s -> y)
->  caseNu s coalg vs)

)


