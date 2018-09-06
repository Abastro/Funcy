import "common/basis/Import"
import -> export {
    import None := import "common/basis/Format"

    Transform := [I, O] #I -> #O

    Self    := [V] Transform(V, V)

    Id      := [V] (V v -> v) inherits Self(V)      // Function definition. Generics are simply function calls

    FromBool := [V] Transform(Bool, V)
    FromInt := [V] Transform(Int, V)
    ToBool  := [V] Transform(V, Bool)
    ToInt := [V] Transform(V, Int)

    ToString := [V] Transform(V, String)

    For -= [F] (F initial, ToBool(F) condition, Self(F) increase) -> #F

    Pair := [T, S] (T left, S right)     // Interface. Could be a type as well

    Consumer := [V] [I] (V value, I input) -> #V // Maps to nothing here - an interface is first shown here. Native means it's fullfiled
    Supplier := [V] [O] #V -> (V value, O output)

    // States
    State := [C] [S] (C const, S var)

    StateT := [C] [S] Consumer(S)(C) consumer -> (
        State(C)(S) state -> (State(C)(S)) (
            state($const),
            consumer(state($var), state($const))
        )
    ) inherits Id(State(C)(S))

    // Wraps
    Wrap := [V] [C] (V value, C content)

    WrapC := [V] [I] Consumer(V)(I) consumer -> (
        Wrap(V)(I) wrapped -> (Wrap(V)(FALSE)) (
            consumer(wrapped($value), wrapped($content)), FALSE
        )
    )

    WrapS := [V] [O] Supplier(V)(O) supplier -> (
        Wrap(V)(?) wrapped ~ {
            pair := supplier(wrapped($value))
        } -> (Wrap(V)(O)) (pair($value), pair($content))
    )

    WrapT := [V] [I, O] Transform(I, O) transform -> (
        Wrap(V)(I) wrapped -> (Wrap(V)(O)) (wrapped($value), transform(wrapped($content)))
    )
}