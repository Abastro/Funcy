import "common.basis.Import"
(import inc) export {
    import None := import "common.basis.format", "common.basis.Types", "common,basis.Pointer", "common.collection.Ites"

    ArrayIte := [F] (
        Int size, Int index, Pointer pointer,
        value -> OffsetGet(F)(value(pointer), index)
    ) inherits Ite(F)     // Mixed interface compound declaration
    NullArrayIte := [F] (0, 0, NullPointer(F)) inherits ArrayIte(F)

    Array := [F] (
        ArrayIte(F) head,
        hasNext : [F] (ArrayIte(F) ite -> ite(size) < ite(index)) inherits Iterable(Ite(F))($hasNext),
        next : [F] ~ {
            I := ArrayIte(F)
            internal := (I ite ~ {
                len := ite(size)
                ind := ite(index) + 1
                newp := OffsetGet(ite(pointer), 1)
            } -> Choose(len < ind)((I) (len, ind, newp), NullIte(F))) inherits Iterable(Ite(F))($next)
        },
        indexer : FromInteger(F),
        setter : Setter(F),
    ) inherits Iterable(Ite(F)), FromInteger(F)

    hid Setter := [F] (ArrayIte(F) ite, Int index, F val) ~ {
        newp := OffsetSet(ite(pointer), index, value)
    } -> (ArrayIte(F)) (ite, pointer -> newp) // Syntax sugar for compound declaration

    Set := [F] (Array(F) array, Integer index, F value) ~ {
        newHead := Setter(array(head), index, value);
    } -> (Array(F)) (array, head -> newHead) // Syntax sugar for compound creation

    NewArray := [F] (func id, Int size) ~ {
        pointer := NewPointer(F)(id, size)
    } -> (Array(F)) (Ite(F)) (size, -1, pointer)

    AsArray := [F] (
        (F a1) -> Set(NewArray(a1), , )
        (F a1, F a2) ~ {
            pointer := NewPointer(F)((p1 : a1, p2 : a2), 2);
        } -> (Array(F)) (Ite(F)),
        (F a1, F a2, F a3) ~ {
            pointer := NewPointer(F)((p1 : a1, p2 : a2, p3 : a3), 3);
            pointer1 := OffsetSet(ite(pointer), 0, a1)
            pointer2 := OffsetSet(ite(pointer), 1, a2)
            pointer3 := OffsetSet(ite(pointer), 2, a3)
        } -> (Array(F)) (Ite(F)) (3, -1, pointer3)
    )
}