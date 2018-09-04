(inc -= &Inclusions) ~ {
    // Need to access non-declared things in Inclusions. But how?
    import {
        #common.basis.Import
        #common.basis.Format
        #common.basis.Types
    }
} -> export {
    // Buffer
    // How to declare an opaque type?
    native OutState -= TRUE                  // OutState needs to be opaque
    Printer := Consumer(OutState, String)
    OutSite := (Printer print, OutState initial)
    native Console -= &OutSite
    Print := [OutSite site] (StringState state, func printed) ->
        site(print)(state, toString(printed))) inherits Consumer(StringState, func)
}