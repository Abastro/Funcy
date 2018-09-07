import "common/format/Import"
import -> export {
    import None := import "common/format/Format"

    Transform := (SI, SO) -> (SI |-> SO)

    // Self Transformation
    Self := [T] Transform(#T, #T)
    Id := [V] (V v -> v) -= %Self(V)      // Function definition. Generics are simply function calls

    // Pair
    Pair := [T, S] (T left, S right)

    // Consumer and Supplier
    Consumer := [V] [I] (V collect, I input) -> #V // Maps to nothing here - an interface is first shown here. Native means it's fullfiled
    Supplier := [V] [O] #V -> (V collect, O output)

}