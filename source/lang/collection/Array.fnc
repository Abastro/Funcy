import "lang/format/Import"
import -> export {
    import None := import "lang/format/Format", "lang/generics/Generics", "lang/basis/Pointer", "lang/collection/Ites";

    /*
     * @arg The type set of the array
     * @ret { arrays }
     */
    Array := F -> #(
        // Array Iterator Implementation
        IteImpl : (
            Int size, Int index,
            Pointer(F) pointer,
            value : OffGet(head, index)($value)
        ),

        // Null Iterator
        NullIte : (0, 0, NullPointer(F)) -= IteImpl,

        // Head
        IteImpl head,

        // Array hasNext Implementation
        hasNext : (ArrayIte(F) ite -> ite(size) < ite(index)),

        // Array next Implementation
        next : (IteImpl ite ~ {
            len := ite(size);
            ind := ite(index) + 1;
            newp := OffGet(ite(pointer), 1);
        } -> Choose(len < ind) ((IteImpl) (len, ind, newp), NullIte),

        indexer : Int index -> OffGet(head($pointer), index)($value)
    ) -= (Iterable(F) & FromInteger(F));

    Set := F -> (Array(F) array, Int index, F value) ~ {
        newp := OffSet(array($head)($pointer), index, value);
        newHead := (Array($IteImpl)) (array($head), pointer : newp);
    } -> (Array(F)) (array, head : newHead);


    NewArray := F -> Int size -> Comp(
        WrapS(NewPointer(F)),
        WrapT(Pointer(F) pointer -> (Array(F)) (Ite(F)) (size, -1, pointer))
    );

    DelArray := F -> Comp(
        WrapT(Array(F) array -> array($head)($pointer)),
        WrapC(DelPointer(F))
    );

    AsArray := F -> (
        (F a1) -> Comp(
            NewArray(F)(1),
            WrapT(Array(F) array -> Set(array, 0, a1))
        ),
        (F a1, F a2) -> Comp(
            NewArray(F)(2),
            WrapT(Comp(
                Array(F) array -> Set(array, 0, a1),
                Array(F) array -> Set(array, 1, a2)
            ))
        ),
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