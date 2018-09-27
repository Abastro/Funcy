"Memory" in "common.memory"
include "common.basis.Import"

import {"common.system.System", "common.generics.Utilities"} ~ {
    Ptr := F -> F value;

    // Evaluates Null Pointer when the type is given.
    NullPtr :: F => Ptr(F);

    MemStack;
    GetStack :: {System} ~> MemStack;

    NewPtr :: F => Supplier(MemStack)(Ptr(F));
    NewArrPtr :: F => ( Int ~> Supplier(MemStack)(Ptr(F)) )

    DelPtr :: F => Consumer(MemStack)(Pointer(F))
    DelArrPtr :: F => ( Int ~> Consumer(MemStack)(Pointer(F)) )


    Set :: F => ( (pointer :: Pointer(F), value :: F) ~> Pointer(F) )
    OffGet :: F => ( (pointer :: Pointer(F), offset :: Int) ~> Pointer(F) )
    OffSet :: F => ( (pointer :: Pointer(F), offset :: Int, value :: F) ~> Pointer(F) )
}