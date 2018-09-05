import "common/basis/Import"
import -> export {
    import None := import "common/basis/Format", "common/basis/Types"
    // In buffer
    InState -= TRUE

    // Opaque OutState
    OutState -= TRUE

    // Out-related interfaces
    Writer := Consumer(OutState, String)
    OutStream := (Writer writer, OutState current)

    Console -= #OutSite

    Print := [OutSite site] (StringState state, func printed) ->
        site(print)(state, toString(printed))) inherits Consumer(StringState, func)
}