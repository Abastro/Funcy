import "lang/format/Import"
import -> export {
    import None := import {"lang/format/Format", "lnag", "lang/generics/Generics", "lang/generics/Commons"}

    /*
     * @arg the type set of Iterator
     * @ret { Set of Iterators. Denotes certain position in the Iterable. }
     */
    Ite := F -> F value;

    /*
     * @arg the type set of Iterable
     * @ret { Prototype relation for Iterables. }
     */
    Iterable := F -> #(
        // Iterator implementation, which is a subset of Ite
        $IteImpl :: @Ite(F),

        // An iterator for the head of this iterable
        $IteImpl head,

        /*
         * @arg the iterator in this iterable
         * @ret if the iterator has next position available
         */
        $hasNext :: ToBool($IteImpl),

        /*
         * @arg the iterator in this iterable
         * @ret the iterator denoting the next position
         */
        $next :: Self($IteImpl)
    );

    // Loops through the iterable.
    Loop := (V, F) -> (
        (Iterable(F) iterable, V initial, Consumer(V, F) consumer) -> For(
            (iterable($head), initial),
            (Ite(F) ite, V val) -> iterable($hasNext)(ite),
            (Ite(F) ite, V val) -> (iterable($next)(ite), consumer(val, ite($value)))
        )
    );
}