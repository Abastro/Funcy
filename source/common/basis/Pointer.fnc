import "common.basis.Import"
import -> export {
    import None := import "common/basis/Format", "common/basis/Types"
    // Definition of Pointer for certain type.
    Pointer := [F] F value

    // Evaluates Null Pointer when the type is given.
    NullPointer -= #(F -> #Pointer(F))

    MemStack -= TRUE
    GetStack -= #Transform(String, MemStack)

    NewPointer -= #[F] #Supplier(MemStack)(Pointer(F))
    NewArrPointer -= #[F] #( #Int -> #Supplier(MemStack)(Pointer(F)) )

    DelPointer -= #[F] #Consumer(MemStack)(Pointer(F))
    DelArrPointer -= #[F] #Int -> #Consumer(MemStack)(Pointer(F))


    Set -= #[F] (Pointer(F) pointer, F value)
    OffGet -= #[F] (Pointer(F) pointer, Int offset) -> #Pointer(F)    // Anonymous compound declaration to easily specify parameters (and result later)
    OffSet -= #[F] (Pointer(F) pointer, Int offset, F value) -> #Pointer(F)


    // Utilities
    GetStackWrap := String id -> ( Wrap(MemStack)(FALSE) ) (GetStack(id), FALSE)
}