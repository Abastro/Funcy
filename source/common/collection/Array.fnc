import "common.basis.Import"
(import inc) export {
    import None := import "common.basis.format", "common.basis.Types", "common,basis.Pointer", "common.collection.Ites"

    Ite := [F] (
        Int size, Int index, Pointer pointer,
        value -> OffsetGet(F)(value(pointer), index)
    ) inherits None::Ite(F)     // Mixed interface compound declaration
    NullIte := [F] (0, 0, NullPointer(F)) inherits Ite(F)

    HasNext := [F] (Ite(F) ite -> ite(size) < ite(index))
        inherits None::HasNext(Ite(F)))    // Inheritance forces the funcy to be applicable for parent cases

    Next := [F] ~ {
        I := Ite(F)
        internal := (I ite ~ {
            len := ite(size)
            ind := ite(index) + 1
            newp := OffsetGet(ite(pointer), 1)
        } -> (len < ind)? (I) (len, ind, newp) : NullIte(F)) inherits None::Next(I)
    } -> internal


    Setter := [F] (Ite(F) ite, Int index, F val) ~ {
        newp := OffsetSet(ite(pointer), index, value)
    } -> (Ite(F)) (ite, pointer -> newp) // Syntax sugar for compound declaration

    Array := [F] (
        Ite(F) head, // The only virtual element
        hasNext : HasNext(F), // Guess it could be implemented just here..
        next : Next(F),
        indexer : FromInteger(F),
        setter : Setter(F),
        indexer expose fi
    ) inherits Iterable(Ite(F)) // Complex compound declaration with inheritance

    Set := [F] (Array(F) array, Integer index, F value) ~ {
        newHead := Setter(array(head), index, value);
    } -> (Array(F)) (array, head -> newHead) // Syntax sugar for compound creation

    AsArray = [F] (F a1, F a2, F a3) ~ {
        pointer := NewPointer(F)((p1 : a1, p2 : a2, p3 : a3), 3);
    } -> (Array) (Ite) (3, -1, pointer)
}