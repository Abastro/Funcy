"Utilities" in "common.generics"
include "lang.format.Import"

import {"commons.generics.Generics"} ~ {
    // Optionals
    hid Optize := T -> (
        (
            theValue :: T | { {} }
        ) -> (
            getOrDef : (def -= T) -> (T) { TRUE : def, FALSE : theValue } (theValue == {}),
            isPresent : theValue != {}
        )
    );

    Optional := T -> Image(Optize(T));
    AsOpt := T -> (T value -> Optize(T)(value));
    NullOpt := T -> Optize(T)({});

    // States
    State := C -> S -> (C const, S var);

    // State Transition
    StateT := C -> S -> Consumer(S)(C) consumer -> (Id(State(C)(S))) (
        State(C)(S) state -> (State(C)(S)) (
            state($const),
            consumer(state($var), state($const))
        )
    );

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
        // Wildcard - union of all the sets
        Wrap(V)(?) wrapped -> {
            pair := supplier(wrapped($value));
        } ~ (Wrap(V)(O)) (pair($value), pair($content))
    );

    // Wrap Transformation
    WrapT := V -> (I, O) -> Transform(I, O) transform -> (
        Wrap(V)(I) wrapped -> (Wrap(V)(O)) (wrapped($value), transform(wrapped($content)))
    );
}