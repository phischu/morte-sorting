-- let
--    wrap = 

(  \(a : *)
->
(  \(L_a : * -> *)
-> \(List_a : *)
-> \(Stream_a : *)
-> \(fmapL : forall (x:*) -> forall (y:*) -> (x -> y) -> L_a x -> L_a y)
-> \(fold : forall (x:*) -> (L_a x -> x) -> List_a -> x)
-> \(wrap : L_a List_a -> List_a)
-> \(unfold : forall (s:*) -> (s -> L_a s) -> s -> Stream_a)
-> \(unwrap : Stream_a -> L_a Stream_a)
-> \(swap : forall (x:*) -> L_a (L_a x) -> L_a (L_a x))
->  fold Stream_a (unfold (L_a Stream_a) (\(s : L_a Stream_a) ->
        swap Stream_a (fmapL Stream_a (L_a Stream_a) unwrap s)))
))


