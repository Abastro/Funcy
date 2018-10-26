func "common.control.Controls", {
    Import, {
        import "common.generics.Generics".
        import "common.generics.Commons".
    }. Import.

    Fn. {
        // Loop statements
        // For
        For, {
            I, ?.
            param, { condition, ToBool I ?. increase, Self I ?. }.

            impl, Self I ?.
            impl, Self I ( {
                input, I ?.
                ret, input, ( param(condition) input ) ( impl param(increse) value ) value;
            } ret).
        }.

            {C, I} -> (
                (condition :: ToBool (State C:I), consumer :: Consumer I:C) -> (Self (State C I)) (
                    state :: State C:I -> For(condition, WrapT consumer)(state)
                )
            )
        );
    };
} Fn.