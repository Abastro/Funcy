"Utilities" in "common.generics"
include "lang.format.Import"

import {"commons.generics.Generics"} ~ {
    // Optionals
    Optional := T -> (
        (
            theValue :: T | { {} }
        ) -> (
            getOrDef : (def :: T) -> As T ( { TRUE : def, FALSE : theValue } (theValue == {}) ),
            isPresent : theValue != {}
        )
    );

    AsOpt := T -> (value :: T -> Optional T value);
    NullOpt := T -> Optional T {};

    // States
    AsState := C -> S -> (
        prop :: func { const :: C; var :: S; } -> { const : prop.const, var : prop.var }
    )
    State := C -> S -> Image(AsState(C)(S));

    // State Transition
    StateT := C -> S -> Consumer(S)(C) consumer -> ( Id(State(C)(S)) ) (
        state :: State(C)(S) -> AsState(C)(S)(
            state.const,
            consumer(state.var, state.const)
        )
    );

    // Wraps
    AsWrap := V -> C -> (
        prop :: func { value :: V; content :: C; } -> { value : prop.value, content : prop.content }
    )
    Wrap := V -> C -> Image(AsWrap(V)(C));

    // Wrap 
    WrapC := V -> I -> Consumer(V)(I) consumer -> (
        wrapped :: Wrap(V)(I) -> AsWrap(V)({FALSE})) (
            consumer(wrapped.value, wrapped.content), FALSE
        )
    );

    // Wrap Expansion
    WrapS := V -> O -> Supplier(V)(O) supplier -> (
        // Wildcard - union of all the sets
        wrapped :: Wrap(V)(?) -> {
            pair := supplier(wrapped($value));
        } ~ AsWrap(V)(O)(pair($value), pair($content))
    );

    // Wrap Transformation
    WrapT := V -> I -> O -> transform :: Transform(I)(O) -> (
        Wrap(V)(I) wrapped -> AsWrap(V)(O)(wrapped($value), transform(wrapped($content)))
    );
}