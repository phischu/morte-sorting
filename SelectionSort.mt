-- Foreign imports
(  \(a : *)
->

-- Module definitions
(  \(L_a : * -> *)
-> \(Mu : (* -> *) -> *)
-> \(Nu : (* -> *) -> *)
-> \(fmapL : forall (x:*) -> forall (y:*) -> (x -> y) -> L_a x -> L_a y)
-> \(fold : forall (f : * -> *) -> forall (x:*) -> (f x -> x) -> Mu f -> x)
-> \(wrap :
        forall (f : * -> *) ->
        (forall (x:*) ->forall (y:*) -> (x -> y) -> f x -> f y) ->
        f (Mu f) ->
        Mu f)
-> \(unfold : forall (f: * -> *) -> forall (s:*) -> (s -> f s) -> s -> Nu f)
-> \(unwrap :
        forall (f : * -> *) ->
        (forall (x:*) -> forall (y:*) -> (x -> y) -> f x -> f y) ->
        Nu f ->
        f (Nu f))
-> \(swap : forall (x:*) -> L_a (L_a x) -> L_a (L_a x))
->  unfold L_a (Mu L_a) (fold L_a (L_a (Mu L_a)) (\(f_x : L_a (L_a (Mu L_a))) ->
        fmapL (L_a (Mu L_a)) (Mu L_a) (wrap L_a fmapL) (swap (Mu L_a) f_x)))
)

-- L_a
(\(x:*) -> forall (z:*) -> z -> (a -> x -> z) -> z)

-- Mu
(\(f : * -> *) -> forall (x:*) -> (f x -> x) -> x)

-- Nu
(\(f : * -> *) -> forall (y:*) -> (forall (s:*) -> (s -> f s) -> s -> y) -> y)

-- fmapL
(   \(x:*)
->  \(y:*)
->  \(f : x -> y)
->  \(lax : forall (z:*) -> z -> (a -> x -> z) -> z)
->  \(z:*)
->  \(empty : z)
->  \(cons : a -> y -> z)
->  lax z empty (\(va:a) -> \(vx:x) -> cons va (f vx))
)

-- fold
(   \(f : * -> *)
->  \(x:*)
->  \(alg : f x -> x)
->  \(mu_f : forall (x:*) -> (f x -> x) -> x)
->  mu_f x alg
)

-- wrap
(
(   \(Mu : (* -> *) -> *)
->  \(fold : forall (f : * -> *) -> forall (x:*) -> (f x -> x) -> Mu f -> x)
->  \(f : * -> *)
->  \(fmap : forall (x:*) -> forall (y:*) -> (x -> y) -> f x -> f y)
->  \(f_mu_f : f (Mu f))
->  \(y:*) ->  \(alg : f y -> y) ->  alg (fmap (Mu f) y (fold f y alg) f_mu_f)
)
(\(f : * -> *) -> forall (x:*) -> (f x -> x) -> x)
(   \(f : * -> *)
->  \(x:*)
->  \(alg : f x -> x)
->  \(mu_f : forall (x:*) -> (f x -> x) -> x)
->  mu_f x alg
)
)

-- unfold
(   \(f : * -> *)
->  \(s : *)
->  \(coalg : s -> f s)
->  \(vs : s)
->  \(y : *)
->  \(caseNu : forall (s:*) -> (s -> f s) -> s -> y)
->  caseNu s coalg vs
)

-- unwrap
(
(   \(Nu : (* -> *) -> *)
->  \(unfold : forall (f: * -> *) -> forall (s:*) -> (s -> f s) -> s -> Nu f)
->  \(f : * -> *)
->  \(fmap : forall (x:*) -> forall (y:*) -> (x -> y) -> f x -> f y)
->  \(nu_f : forall (y:*) -> (forall (s:*) -> (s -> f s) -> s -> y) -> y)
->  nu_f (f (Nu f)) (\(s:*) -> \(coalg : s -> f s) -> \(vs:s) -> fmap s (Nu f) (unfold f s coalg) (coalg vs))
)
(\(f : * -> *) -> forall (y:*) -> (forall (s:*) -> (s -> f s) -> s -> y) -> y)
(   \(f : * -> *)
->  \(s : *)
->  \(coalg : s -> f s)
->  \(vs : s)
->  \(y : *)
->  \(caseNu : forall (s:*) -> (s -> f s) -> s -> y)
->  caseNu s coalg vs
)
)


)
