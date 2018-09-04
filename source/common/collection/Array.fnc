(inc -= &Inclusions) ~ {
    import inc(#common.basis.Import),
    import inc(#common.basis.Format),
    import inc(#common.basis.Types),
    import inc(#common.basis.Pointer),
    import inc(#common.collection.Ites) as None
} -> export {
    Ite := [F] (
        Int size, Int index, Pointer pointer,
        value -> OffsetGet(F)(value(pointer), index)
    ) inherits None::Ite(F)     // Mixed interface compound declaration
    NullIte := [F] (0, 0, NullPointer(F)) inherits Ite(F)

    HasNext := [F] Ite(F) ite -> ite(size) < ite(index)
        inherits None::HasNext(Ite(F)))    // Inheritance forces the funcy to be applicable for parent cases

    Next := [F] ~ { // For declarations depending on the parameter
        I := Ite(F)
        internal := (I ite ~ {
            len := ite(size)
            ind := ite(index) + 1
            newp := OffsetGet(ite(pointer), 1)              // Auto-evaluated compound from funcy call
        } -> (len < ind)? (I) (len, ind, newp) : NullIte(F)) inherits None::Next(I)
    } -> internal


    Setter := [F] (Ite(F) ite, Int index, F val) ~ {
        newp := OffsetSet(ite(pointer), index, value)
    } -> (Ite(F)) (ite, pointer -> newp) // Syntax sugar for compound declaration

Array := [F] (
    Array::Ite(F) head, // The only virtual element
    hasNext -> Array::HasNext(F), // Guess it could be implemented just here..
    next -> Array::Next(F),
    indexer -> FromInteger(F),
    setter -> Setter(F),
    indexer expose fi
) inherits Iterable(Array::Ite(F)) // Complex compound declaration with inheritance

Set := [F] (Array(F) array, Integer index, F value) ~ {
    newHead := Setter(array(head), index, value);
} -> (Array(F)) (array, head -> newHead) // Syntax sugar for compound creation
}