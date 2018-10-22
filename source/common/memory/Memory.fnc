"Memory" in "common.memory"
include "common.basis.Import"

import {"common.system.System", "common.generics.Utilities"} ~ {
    Ptr : \F = \ptr : (value : F ?) = ptr;

    // Evaluates Null Pointer when the type is given.
    NullPtr : \F = Ptr F ?;

    MemStack;
    GetStack : (System : MemStack ?);

    NewPtr : \F = Supplier MemStack (Ptr F) ?;
    NewArrPtr : \F = \size : Int ? = Supplier MemStack (Ptr F) ?;

    DelPtr : \F = Consumer MemStack (Ptr F) ?;
    DelArrPtr : \F = \size : Int ? = Consumer MemStack (Ptr F) ?;


    Set : \F = Consumer (Ptr F) F ?;
    OffGet : \F = \offset : Int ? = Self (Ptr F) ?;
    OffSet : \F = \offset : Int ? = Consumer (Ptr F) F ?;
}