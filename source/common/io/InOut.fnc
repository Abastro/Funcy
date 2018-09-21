"InOut" in "common.io"
include "lang.format.Import"

import {"lang.generics.Generics", "lang.generics.Utilities"} ~ {
    // Opaque InState..?
    InState;

    // Opaque OutState..?
    OutState;

    // Out-related interfaces
    Writer := Consumer(OutState, String);
    OutStream := State(Writer, OutState);

    Print := T -> (ToString(T) toString) -> (Consumer(OutStream, T)) (
        (OutStream stream, T toPrint) -> (
            // Transition of the stream
            StateT(
                (OutState state, Writer writer) -> writer(state, toString(toPrint))
            )(stream)
        )
    )
}
