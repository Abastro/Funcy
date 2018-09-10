import "common/format/Import"
import -> export {
    import None := import "common/format/Format";

    Transform := (SI, SO) -> (SI |-> SO);

    // Self Transformation
    Self := [T] Transform(#T, #T);
    Id := [V] (V v -> v) -= #Self(V);

    // Pair
    Pair := [T, S] (T left, S right);

    // Consumer and Supplier
    Consumer := [V] [I] (V collect, I input) -> #V;
    Supplier := [V] [O] #V -> (V collect, O output);

}