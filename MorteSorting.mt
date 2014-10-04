-- Foreign imports
(  \(a : *)
-> \(min : a -> a -> a)
-> \(max : a -> a -> a)
->

-- Module definitions
(  \(L_a : * -> *)
-> \(Mu : (* -> *) -> *)
-> \(Nu : (* -> *) -> *)
-> \(fmapL :
        forall (x:*) ->
        forall (y:*) ->
        (x -> y) ->
        L_a x ->
        L_a y)
-> \(fold :
        forall (f : * -> *) ->
        forall (x:*) ->
        (f x -> x) ->
        Mu f -> x)
-> \(wrap :
        forall (f : * -> *) ->
        (forall (x:*) ->forall (y:*) -> (x -> y) -> f x -> f y) ->
        f (Mu f) ->
        Mu f)
-> \(unfold :
        forall (g: * -> *) ->
        forall (s:*) ->
        (s -> g s) ->
        s ->
        Nu g)
-> \(unwrap :
        forall (g : * -> *) ->
        (forall (x:*) -> forall (y:*) -> (x -> y) -> g x -> g y) ->
        Nu g ->
        g (Nu g))
-> \(swap :
        forall (x:*) ->
        L_a (L_a x) ->
        L_a (L_a x))
->
--  Insertion sort
    fold L_a (Nu L_a) (unfold L_a (L_a (Nu L_a)) (\(s : L_a (Nu L_a)) ->
        swap (Nu L_a) (fmapL (Nu L_a) (L_a (Nu L_a)) (unwrap L_a fmapL) s)))
--  Selection sort
--  unfold L_a (Mu L_a) (fold L_a (L_a (Mu L_a)) (\(f_x : L_a (L_a (Mu L_a))) ->
--      fmapL (L_a (Mu L_a)) (Mu L_a) (wrap L_a fmapL) (swap (Mu L_a) f_x)))
)

-- type L_a x = z -> (a -> x -> z) -> z
(\(x:*) -> forall (z:*) -> z -> (a -> x -> z) -> z)

-- data Mu f = Mu (forall x . (f x -> x) -> x)
(\(f : * -> *) -> forall (x:*) -> (f x -> x) -> x)

-- data Nu g = forall s . Nu (s -> g s) s
(\(g : * -> *) -> forall (y:*) -> (forall (s:*) -> (s -> g s) -> s -> y) -> y)

-- fmapL :: (x -> y) -> L_a x -> L_a y
-- fmapL f la_x empty cons = la_x empty (\va vx -> cons va (f vx))
(   \(x:*)
->  \(y:*)
->  \(f : x -> y)
->  \(la_x : forall (z:*) -> z -> (a -> x -> z) -> z)
->  \(z:*)
->  \(empty : z)
->  \(cons : a -> y -> z)
->  la_x z empty (\(va:a) -> \(vx:x) -> cons va (f vx))
)

-- fold :: (f x -> x) -> Mu f -> x
-- fold alg (Mu mu_f) = mu_f alg
(   \(f : * -> *)
->  \(x:*)
->  \(alg : f x -> x)
->  \(mu_f : forall (x:*) -> (f x -> x) -> x)
->  mu_f x alg
)

-- wrap :: f (Mu f) -> Mu f
-- wrap f_mu_f = Mu (\alg -> alg (fmap (fold alg) f_mu_f)))
(
(   \(Mu : (* -> *) -> *)
->  \(fold : forall (f : * -> *) -> forall (x:*) -> (f x -> x) -> Mu f -> x)
->  \(f : * -> *)
->  \(fmap : forall (x:*) -> forall (y:*) -> (x -> y) -> f x -> f y)
->  \(f_mu_f : f (Mu f))
->  \(y:*) ->  \(alg : f y -> y) ->  alg (fmap (Mu f) y (fold f y alg) f_mu_f)
)
-- Mu
(\(f : * -> *) -> forall (x:*) -> (f x -> x) -> x)
-- fold
(   \(f : * -> *)
->  \(x:*)
->  \(alg : f x -> x)
->  \(mu_f : forall (x:*) -> (f x -> x) -> x)
->  mu_f x alg
)
)

-- unfold :: (s -> g s) -> s -> Nu g
-- unfold coalg vs = Nu coalg vs
(   \(g : * -> *)
->  \(s : *)
->  \(coalg : s -> g s)
->  \(vs : s)
->  \(y : *)
->  \(caseNu : forall (s:*) -> (s -> g s) -> s -> y)
->  caseNu s coalg vs
)

-- unwrap :: Nu g -> g (Nu g)
-- unwrap (Nu coalg vs) = fmap (unfold coalg) (coalg vs)
(
(   \(Nu : (* -> *) -> *)
->  \(unfold : forall (g: * -> *) -> forall (s:*) -> (s -> g s) -> s -> Nu g)
->  \(g : * -> *)
->  \(fmap : forall (x:*) -> forall (y:*) -> (x -> y) -> g x -> g y)
->  \(nu_g : forall (y:*) -> (forall (s:*) -> (s -> g s) -> s -> y) -> y)
->  nu_g (g (Nu g)) (\(s:*) -> \(coalg : s -> g s) -> \(vs:s) -> fmap s (Nu g) (unfold g s coalg) (coalg vs))
)
-- Nu
(\(g : * -> *) -> forall (y:*) -> (forall (s:*) -> (s -> g s) -> s -> y) -> y)
-- unfold
(   \(g : * -> *)
->  \(s : *)
->  \(coalg : s -> g s)
->  \(vs : s)
->  \(y : *)
->  \(caseNu : forall (s:*) -> (s -> g s) -> s -> y)
->  caseNu s coalg vs
)
)

-- swap :: L_a (L_a x) -> L_a (L_a x)
-- swap Empty = Empty
-- swap (Cons a Empty) = Cons a Empty
-- swap (Cons a1 (Cons a2 x)) = Cons (min a1 a2) (Cons (max a1 a2) x)
(
(   \(L_a : * -> *)
->  \(empty : forall (x:*) -> L_a x)
->  \(cons : forall (x:*) -> a -> x -> L_a x)
->  \(caseLa : forall (x:*) -> L_a x -> forall (z:*) -> z -> (a -> x -> z) -> z)
->  \(x:*)
->  \(outer : L_a (L_a x))
->  caseLa (L_a x) outer (L_a (L_a x))
        (empty (L_a x))
        (\(vaa : a) -> \(inner : L_a x) -> caseLa x inner (L_a (L_a x))
            (cons (L_a x) vaa (empty x))
            (\(vab : a) -> \(vx : x) ->
                (cons (L_a x) (min vaa vab) (cons x (max vaa vab) vx))))

)
-- L_a
(\(x:*) -> forall (z:*) -> z -> (a -> x -> z) -> z)
-- empty
(\(x:*) -> \(z:*) -> \(empty : z) -> \(cons : a -> x -> z) -> empty)
-- cons
(\(x:*) -> \(head : a) -> \(tail : x) -> \(z:*) -> \(empty : z) -> \(cons : a -> x -> z) -> cons head tail)
-- caseLa
(\(x:*) -> \(lax : forall (z:*) -> z -> (a -> x -> z) -> z) -> lax)
)

--(\(x:*) -> \(f : (forall (z:*) -> z -> (a -> x -> z) -> z) -> x) -> f (\(z:*) -> \(empty : z) -> \(cons : a -> x -> z) -> empty))

)
