import "common/basis/Import"
import -> export {
    import None := import "common/basis/Format", "common/basis/Types"

    // Iterators
    Ite := [F] F value     // Virtual Funcy declared as a virtual compound

    Iterable := [Ite(?) I] (
        I head,
        hasNext :: #I -> #Bool,
        next :: #I -> #I
    )

    ItePair := [F, V] Pair(Ite(F) ite, V val)
    // Parameter is automatically wrapped into compound
    Loop := [V, F] (Iterable(F) iterable, V initial, Consumer(V, F) consumer) -> For(
        Pair(iterable($head), initial),
        (Ite(F) ite, V val) -> iterable($hasNext)(ite),
        (Ite(F) ite, V val) -> (iterable($next)(ite), consumer(val, ite(value)))
    ) // Auto-complete things here
}