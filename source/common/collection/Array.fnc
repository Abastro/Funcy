"Array" in "common.collection"

include "lang.format.Import"
{
    import None := import {"lang.format.Format", "common.generics.Generics", "common.memory.Memory", "common.collection.Ites"};

    /*
     * @arg The type set of the array
     * @ret { Set of arrays on the type }
     */
    Array := F -> (
        // Length
        Int length,

        // Head pointer
        Ptr(F) headPtr,

        // Array Iterator Implementation
        IteImpl : (
            Int index,
            value : OffGet(headPtr, index)($value)
        ),

        // Null Iterator
        NullIte : (IteImpl) (length),

        // Head
        head : (IteImpl) (0),

        // Array hasNext Implementation
        hasNext : ( IteImpl ite -> ite(index) < length ),

        // Array next Implementation
        next : IteImpl ite -> {
            ind := ite(index) + 1;
        } ~ Choose(ind < length) ((IteImpl) (ind), NullIte),

        indexer : Int index -> OffGet(headPtr, index)($value)
    ) -= (Iterable(F) & FromInt(F));

    // Set function
    Set := F -> (#Array(F) array, Int index, F value) -> {
        newp := OffSet(array($head)($pointer), index, value);
        newHead := (array($IteImpl)) (pointer : newp, array($head));
    } ~ (Array(F)) (head : newHead, array);


    // Creates an array
    NewArray := F -> Int size -> Comp(
        WrapS(NewPtr(F)),
        WrapT(Ptr(F) pointer -> (Array(F)) (Ite(F)) (size, -1, pointer))
    );

    // Deletes an array
    DelArray := F -> Comp(
        WrapT(Array(F) array -> array($head)($pointer)),
        WrapC(DelPtr(F))
    );

    AsArray := F -> (
        // Single parameter
        (F a1) -> Comp(
            NewArray(F)(1),
            WrapT(Array(F) array -> Set(array, 0, a1))
        ),

        // Double parameter
        (F a1, F a2) -> Comp(
            NewArray(F)(2),
            WrapT(Comp(
                Array(F) array -> Set(array, 0, a1),
                Array(F) array -> Set(array, 1, a2)
            ))
        ),

        // Triple parameter
        (F a1, F a2, F a3) -> Comp(
            NewArray(F)(3),
            WrapT(Comp(
                Array(F) array -> Set(array, 0, a1),
                Array(F) array -> Set(array, 1, a2),
                Array(F) array -> Set(array, 2, a3)
            ))
        ),
    );
}