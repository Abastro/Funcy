"Memory" in "common.memory"
include "common.basis.Import"

import {"common.system.System", "common.generics.Utilities"} ~ {
    Ptr := F -> F value;

    // Evaluates Null Pointer when the type is given.
    NullPtr -= F => Ptr(F);

    MemStack;
    GetStack -= {System} ~> MemStack;

    NewPtr -= F => Supplier(MemStack)(Ptr(F));
    NewArrPtr -= F => ( Int ~> Supplier(MemStack)(Ptr(F)) )

    DelPtr -= F => Consumer(MemStack)(Pointer(F))
    DelArrPtr -= F => ( Int ~> Consumer(MemStack)(Pointer(F)) )


    Set -= F => ( (Pointer(F) pointer, F value) ~> Pointer(F) )
    OffGet -= F => ( (Pointer(F) pointer, Int offset) ~> Pointer(F) )
    OffSet -= F => ( (Pointer(F) pointer, Int offset, F value) ~> Pointer(F) )
}