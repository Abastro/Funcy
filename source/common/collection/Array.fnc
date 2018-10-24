"Array" in "common.collection"

{
    import [
        "common.generics.Generics",
        "common.generics.Utilities",
        "common.control.Constrols",
        "common.memory.Memory",
        "common.collection.Ites"
    ];

    SizedArray : T -> (
        length : Int ? ->
        headPtr : Ptr F ? -> {
            ElGetter : (FromInt T) (index :: Int ?) -> (OffGet index props.headPtr).value;
            IterFor : (pIndex : Int ?) -> (index : pIndex);
        } ~ AsIterable T IterFor {
            // Inheritance Definition
            head : IterFor 0;
            next : ( ite : IteFor ? -> {
                ind : ite.index + 1
            } ~ Choose(Optional IterFor) (ind < props.length) ( AsOpt (IterFor ind), NullOpt IterFor );
        } | {
            // Exposed Methods
            length : props.length;
        } | ElGetter;
    );

    // Array type definition
    Array := T -> con :: TypeP Int (Ptr F) -> ArrayWith (InP con) (OutP con);

    Set := F -> pair : TypeP Int F -> Self (Array F) ( array :: Array F -> {
        newPtr := OffSet InP pair (array.head.pointer : OutP pair)
    });

    // Set function
    Set := F -> param :: func { array :: Array F; index :: Int; value :: F; } -> {
        newPtr := OffSet(param.array.head.pointer, param.index, param.value);
    } ~ ArrayWith(param.array.length, newPtr);


    // Creates an array
    NewArray := F -> (size :: Int) -> Comp(
        WrapS NewPtr F,
        WrapT(pointer :: Ptr F -> ArrayWith(size, pointer))
    );

    // Deletes an array
    DelArray := F -> Comp(
        WrapT(array :: Array F -> array.head.pointer),
        WrapC DelPtr F
    );

    AsArray := F -> (
        (a :: F) -> Comp(
            NewArray F 1,
            WrapT(array :: Array F -> Set(array, 0, a))
        ) |
        // Double parameter
        (a1 :: F, a2 :: F) -> Comp(
            NewArray F 2,
            WrapT Comp(
                array :: Array F -> Set(array, 0, a1),
                array :: Array F -> Set(array, 1, a2)
            )
        ) |
        // Triple parameter
        (a1 :: F, a2 :: F, a3 :: F) -> Comp(
            NewArray F 3,
            WrapT Comp(
                array :: Array F -> Set(array, 0, a1),
                array :: Array F -> Set(array, 1, a2),
                array :: Array F -> Set(array, 2, a3)
            )
        ),
    );
}