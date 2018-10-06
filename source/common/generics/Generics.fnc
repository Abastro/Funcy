"Generics" in "common.generics"

// NOTE: Now going back to functional language
{
    Transform := spec :: Typed I:O ? -> I ~> O;
    Self := T -> Transform T:T;

    // Identity
    Id := T -> As (Self T) (v :: T -> v);

    // Consumer and Supplier
    Consumer := V -> I -> Transform Typed(V:I) : V;
    Supplier := V -> O -> Transform V : Typed(V:O);
}