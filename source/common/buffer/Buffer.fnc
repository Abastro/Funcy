import "common/basis/Import"
import -> export {
    import None := import "common/basis/Format", "common/basis/Types"
    // In buffer
    InState -= TRUE

    // Out Buffer
    // How to declare an opaque type?
    OutState -= TRUE                  // OutState needs to be opaque
    Printer := Consumer(OutState, String)
    OutSite := (Printer print, OutState initial)
    Console -= &OutSite
    Print := [OutSite site] (StringState state, func printed) ->
        site(print)(state, toString(printed))) inherits Consumer(StringState, func)
}