import "lang/format/Import"
import -> export {
    import None := import {"lang/format/Format", "lang/generics/Generics", "lang/generics/Utilities"}
    // Opaque InState
    InState -= {@}

    // Opaque OutState
    OutState -= {@}

    // Out-related interfaces
    Writer := Consumer(OutState, String)
    OutStream := State(Writer, OutState)

    Print := T -> (ToString(T) toString) -> (
        (OutStream stream, T toPrint) -> StateT(
            (OutState state, Writer writer) -> writer(state, toString(toPrint))
        )(stream)
    ) -= Consumer(OutStream, T)
}
