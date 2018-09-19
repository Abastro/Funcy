include "lang.format.Import"
{
    import None := import {"lang.format.Format", "commons.generics.Generics"}

    // Optionals
    Optional := T -> (
        value <: T | { {} },
        getOrDef : T def -> (T) ( TRUE : def, FALSE : value )(value == {})
    );

    // States
    State := C -> S -> (C const, S var);

    // State Transition
    StateT := C -> S -> Consumer(S)(C) consumer -> (Id(State(C)(S))) (
        State(C)(S) state -> (State(C)(S)) (
            state($const),
            consumer(state($var), state($const))
        )
    ) -= Id(State(C)(S));

    // Wraps
    Wrap := V -> C -> (V value, C content);

    // Wrap 
    WrapC := V -> I -> Consumer(V)(I) consumer -> (
        Wrap(V)(I) wrapped -> (Wrap(V)(FALSE)) (
            consumer(wrapped($value), wrapped($content)), FALSE
        )
    );

    // Wrap Expansion
    WrapS := V -> O -> Supplier(V)(O) supplier -> (
        Wrap(V)(?) wrapped -> {
            pair := supplier(wrapped($value));
        } ~ (Wrap(V)(O)) (pair($value), pair($content))
    );

    // Wrap Transformation
    WrapT := V -> (I, O) -> Transform(I, O) transform -> (
        Wrap(V)(I) wrapped -> (Wrap(V)(O)) (wrapped($value), transform(wrapped($content)))
    );
}