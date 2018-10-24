"Controls" in "common.control" {
    hid import ["common.generics.Generics", "common.generics.Commons"];

    // Conditional Statements
    // Either
    Either. \T. \param = Pair (T:T) ?. FromBool T {
        TRUE. InOf param;
        FALSE. OutOf param;
    };

    // Choose
    Choose. \T. FromBool (Function ( Pair(T:T):T )) ?;
    Choose. \T. {
        TRUE. \param = Pair (T:T) ?. InOf param;
        FALSE. \param = Pair (T:T) ?. OutOf param;
    };

    // Loop statements
    // For
    For. \I. \param = { condition. ToBool I ?; increase. Self I ?; }. {
        // Looper Declaration
        impl. Self I ?;

        // Looper Definition
        impl. Self I ( \value = I ?. Choose (param.condition value) (impl (param.increase value) : value) );
    } impl;

        {C, I} -> (
            (condition :: ToBool (State C:I), consumer :: Consumer I:C) -> (Self (State C I)) (
                state :: State C:I -> For(condition, WrapT consumer)(state)
            )
        )
    );
}