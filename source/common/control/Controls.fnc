"Controls" in "common.control"
include "lang.format.Import"

import {"lang.generics.Generics", "lang.generics.Commons"} ~ {
    // Conditional Statements
    // Either
    Either : T -> param : TypeP T:T ? -> FromBool T {
        TRUE : InP param;
        FALSE : OutP param;
    };

    // Choose
    Choose : T -> (flag : Bool ?) -> (
        param : TypeP T:T ? -> Either param flag
    );

    // Loop statements
    // For
    For : (
        I -> param : { condition : ToBool I ?; increase : Self I ?; } -> {
            // Looper Declaration
            impl : Self I ?;

            // Looper Definition
            impl : Self I ( (value : I ?) -> Choose (param.condition value) (impl (param.increase value) : value) );
            }
        } ~ impl
    );

        {C, I} -> (
            (condition :: ToBool (State C:I), consumer :: Consumer I:C) -> (Self (State C I)) (
                state :: State C:I -> For(condition, WrapT consumer)(state)
            )
        )
    );
}