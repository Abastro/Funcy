import "lang/format/Import"
import -> export {
    import None := import "lang/format/Format", "lang/generics/Generics", "lang/basis/Pointer", "lang/collection/Ites";

    ArrayIte := [F] (
        Int size,
        Int index,
        Pointer(F) head,
        value : OffGet(F)(head, index)($value)
    ) inherits Ite(F);     // Mixed interface compound declaration
    NullArrayIte := [F] (0, 0, NullPointer(F)) inherits ArrayIte(F);

    hid Setter := [F] (ArrayIte(F) ite, Int index, F val) ~ {
        newp := OffSet(ite(pointer), index, value);
    } -> (ArrayIte(F)) (ite, pointer -> newp); // Syntax sugar for compound declaration

    Array := [F] (
        ArrayIte(F) head,

        hasNext : [F] (ArrayIte(F) ite -> ite(size) < ite(index)),

        next : [F] ~ {
            I := ArrayIte(F);
        } -> (I ite ~ {
            len := ite(size);
            ind := ite(index) + 1;
            newp := OffGet(ite(pointer), 1);
        } -> Choose(len < ind) ((I) (len, ind, newp), NullArrayIte(F))),

        indexer : [F]  FromInteger(F),
        setter : (ArrayIte(F) ite, Int index, F val),
    ) inherits Iterable(Ite(F)), FromInteger(F);

    Set := [F] (Array(F) array, Integer index, F value) ~ {
        newHead := Setter(array(head), index, value);
    } -> (Array(F)) (array, head -> newHead);


    NewArray := [F] Int size -> Comp(
        WrapS(NewPointer(F)),
        WrapT(Pointer(F) pointer -> (Array(F)) (Ite(F)) (size, -1, pointer))
    );

    DelArray := [F] Comp(
        WrapT(Array(F) array -> array($head)($pointer)),
        WrapC(DelPointer(F))
    );

    AsArray := [F] (
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