"Controls" in "common.control"
include "lang.format.Import"

import {"lang.generics.Generics", "lang.generics.Commons"} ~ {
    // Conditional Statements
    // Either
    Either := T -> (forTrue :: T, forFalse :: T) -> (FromBool(T)) {
        TRUE : forTrue,
        FALSE : forFalse
    };

    // Choose
    Choose := T -> (flag :: T) -> (
        (forTrue :: T, forFalse :: T) -> Either(forTrue, forFalse)(flag)
    );

    // Loop statements
    // For
    For := (
        I -> (condition :: ToBool(I), increase :: Self(I)) -> {
            // Looper Declaration
            impl :: Self(I);

            // Looper Definition
            impl := I value -> Choose(condition(value)) (
                impl(increase(value)),
                value
            );
        } ~ impl,
        (C, I) -> (
            (condition :: ToBool(State(C)(I)), consumer :: Consumer(I)(C)) -> (Self(State(C)(I))) (
                state :: State(C)(I) -> For(condition, WrapT(consumer))(state)
            )
        )
    );
}