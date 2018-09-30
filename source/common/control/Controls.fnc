"Controls" in "common.control"
include "lang.format.Import"

import {"lang.generics.Generics", "lang.generics.Commons"} ~ {
    // Conditional Statements
    // Either
    Either := T -> param :: { forTrue :: T; forFalse :: T; } -> (FromBool(T)) {
        TRUE : param.forTrue,
        FALSE : param.forFalse
    };

    // Choose
    Choose := T -> (flag :: T) -> (
        param :: { forTrue :: T; forFalse :: T; } -> Either(param)(flag)
    );

    // Loop statements
    // For
    For := (
        I -> param :: { condition :: ToBool(I); increase :: Self(I); } -> {
            // Looper Declaration
            impl :: Self(I);

            // Looper Definition
            impl := I value -> Choose(param.condition(value)) (
                impl(param.increase(value)),
                value
            );
        } ~ impl
    );

        {C, I} -> (
            (condition :: ToBool(State(C)(I)), consumer :: Consumer(I)(C)) -> (Self(State(C)(I))) (
                state :: State(C)(I) -> For(condition, WrapT(consumer))(state)
            )
        )
    );
}