func "common.io.InOut", {
    Import, {
        import "common.generics.Generics".
        import "common.generics.Utilities".
    }. Import.

    Fn, {

        Print, {
            InputType, ?.
            toString, ToString InputType ?.

            print, Consumer OutStream InputType ( {
                stream, OutStream ?.
                input, T ?.

                consumer, stream, input, StateT().
            } consumer).
        } print.
    }.
}.
