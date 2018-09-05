import "common.basis.Import"
import -> export {
    import None := import "common/basis/Format", "common/basis/Types"
    // Definition of Pointer for certain type.
    Pointer := [F] F value

    // Evaluates Null Pointer when the type is given.
    NullPointer -= #(F -> #Pointer(F))
    NewPointer -= #[F] (
        id -> #Pointer(F),
        (id, Int size) -> #Pointer(F)
    )

    OffsetGet -= #[F] (Pointer(F) pointer, Int offset) -> #Pointer(F)    // Anonymous compound declaration to easily specify parameters (and result later)
    OffsetSet -= #[F] (Pointer(F) pointer, Int offset, V value) -> #Pointer(F)
}