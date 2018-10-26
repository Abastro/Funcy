"Memory" in "common.memory"
include "common.basis.Import"
func "common.memory.Memory", {
    Import, {
        import "common.system.System".
        import "common.generics.Utilities".
    }. Import.

    Fn. {
        Ptr, { F, ?. input, F ?. ptr, F, input, (Value, input). } ptr.

        MemStack, Exist ?.
        GetStack, System, MemStack ?.

        NewPtr, { F, ?. newPtr, F, Supplier MemStack (Ptr F) ?. } newPtr.
        DelPtr, { F, ?. delPtr, F, Consumer MemStack (Ptr F) ?. } delPtr.

        Size, { literal, Int ?. size, literal, ?. } size.

        NewArrPtr : \F = \size : Int ? = Supplier MemStack (Ptr F) ?;

        DelPtr : \F = Consumer MemStack (Ptr F) ?;
        DelArrPtr : \F = \size : Int ? = Consumer MemStack (Ptr F) ?;


        Set : \F = Consumer (Ptr F) F ?;
        OffGet : \F = \offset : Int ? = Self (Ptr F) ?;
        OffSet : \F = \offset : Int ? = Consumer (Ptr F) F ?;
    }
} Fn.