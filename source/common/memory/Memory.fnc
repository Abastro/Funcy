"Memory" in "common.memory"
include "common.basis.Import"

import {"common.system.System", "common.generics.Utilities"} ~ {
    Ptr := F -> value : F;

    // Evaluates Null Pointer when the type is given.
    NullPtr :: F => Ptr(F);

    MemStack;
    GetStack :: {System} ~> MemStack;

    NewPtr :: F => Supplier MemStack:Ptr(F);
    NewArrPtr :: F => ( Int ~> Supplier MemStack:Ptr(F) )

    DelPtr :: F => Consumer MemStack:Ptr(F)
    DelArrPtr :: F => ( Int ~> Consumer MemStack:Ptr(F) )


    Set :: F => ( (pointer :: Ptr F, value :: F) ~> Ptr F )
    OffGet :: F => ( (pointer :: Ptr F, offset :: Int) ~> Ptr F )
    OffSet :: F => ( (pointer :: Ptr F, offset :: Int, value :: F) ~> Ptr F )
}