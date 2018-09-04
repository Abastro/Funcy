(inc -= &Inclusions) ~ {
    import (
        #common.basis.Import,
        #common.basis.Format,
        #common.basis.Types
    )
} -> export {
    // Definition of Pointer for certain type - it is defined to direct the value.
    Pointer := F -> ($value -> &F)

    // Evaluates Null Pointer when the type is given.
    NullPointer -= F -> Pointer(F)
    NewPointer -= [F] (func id) -> % Pointer(F)
    NewPointer -= [F] (func id, Int size) -> % Pointer(F)     // Can assign another when the Parameter Type anywhere is different

    OffsetGet -= [F] (Pointer(F) pointer, Int offset) -> % Pointer(F)    // Anonymous compound declaration to easily specify parameters (and result later)
    OffsetSet -= [F] (Pointer(F) pointer, Int offset, V value) -> % Pointer(F)
}