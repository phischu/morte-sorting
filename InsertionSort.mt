-- Foreign imports
(  \(a : *)
->

-- Module definitions
(  \(L_a : * -> *)
-> \(Mu : (* -> *) -> *)
-> \(Nu : (* -> *) -> *)
-> \(fmapL : forall (x:*) -> forall (y:*) -> (x -> y) -> L_a x -> L_a y)
-> \(fold : forall (f: * -> *) -> forall (x:*) -> (f x -> x) -> Mu f -> x)
-> \(wrap : L_a (Mu L_a) -> Mu L_a)
-> \(unfold : forall (s:*) -> (s -> L_a s) -> s -> Nu L_a)
-> \(unwrap : Nu L_a -> L_a (Nu L_a))
-> \(swap : forall (x:*) -> L_a (L_a x) -> L_a (L_a x))
->  fold L_a (Nu L_a) (unfold (L_a (Nu L_a)) (\(s : L_a (Nu L_a)) ->
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
(\(f: * -> *) -> \(x:*) -> \(alg : f x -> x) -> \(mu_f : forall (x:*) -> (f x -> x) -> x) -> mu_f x alg)

)


