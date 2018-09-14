import "lang/format/Import"
import -> export {
    import None := import "lang/format/Format", "lang/generics/Generics"

    // Optionals
    Optional := T -> (
        $value :: T | { {} },
        $getOrDef : (
            T def -> (
                TRUE : ,
                FALSE : 
            )($value == {})
        )
    );

    // States
    State := [C] [S] (C $const, S $var);

    // State Transition
    StateT := [C] [S] Consumer(S)(C) consumer -> (
        State(C)(S) state -> (State(C)(S)) (
            state($const),
            consumer(state($var), state($const))
        )
    ) inherits Id(State(C)(S));

    // Wraps
    Wrap := [V] [C] (V value, C content);

    // Wrap 
    WrapC := [V] [I] Consumer(V)(I) consumer -> (
        Wrap(V)(I) wrapped -> (Wrap(V)(FALSE)) (
            consumer(wrapped($value), wrapped($content)), FALSE
        )
    );

    // Wrap Expansion
    WrapS := [V] [O] Supplier(V)(O) supplier -> (
        Wrap(V)(?) wrapped ~ {
            pair := supplier(wrapped($value));
        } -> (Wrap(V)(O)) (pair($value), pair($content))
    );

    // Wrap Transformation
    WrapT := [V] [I, O] Transform(I, O) transform -> (
        Wrap(V)(I) wrapped -> (Wrap(V)(O)) (wrapped($value), transform(wrapped($content)))
    );
}