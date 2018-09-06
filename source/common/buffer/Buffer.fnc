import "common/basis/Import"
import -> export {
    import None := import "common/basis/Format", "common/basis/Types"
    // In buffer
    InState -= TRUE

    // Opaque OutState
    OutState -= TRUE

    // Out-related interfaces
    Writer := Consumer(OutState, String)
    OutStream := State(Writer, OutState)

    GetConsole -= #(#String -> #OutStream)

    Print := [T] ToString(T) toString -> (
        (OutStream stream, T toPrint) -> StateT(
            (OutState state, Writer writer) -> writer(state, toString(toPrint))
        )(stream)
    ) inherits Consumer(OutStream, T)
}
