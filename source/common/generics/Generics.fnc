"Generics" in "common.generics"

{
    hid AsPair := I -> O -> param :: func { left :: I; right :: O; } -> left : right;
    hid TypePair := I -> O -> Image(AsPair(I)(O));

    Left := I -> O -> TypePair(I)(O)

    Transform := Pair(I:O) -> Power(Pair(I:O));
    Self := T -> Transform(T:T);

    // Identity
    Id := T -> (Self(T)) (v :: T -> v);

    // Consumer and Supplier
    Consumer := V -> I -> Transform (Pair(V:I) : V);
    Supplier := V -> O -> Transform (V : Pair(V:O));
}