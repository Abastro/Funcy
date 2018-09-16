import "lang/format/Import"
import -> export {
    import None := import {"lang/format/Format", "lang/generics/Generics", "lang/generics/Commons"}

    /*
     * @arg the type set of Iterator
     * @ret { Set of Iterators. Denotes certain position in the Iterable. }
     */
    Ite := T -> T value;

    /*
     * @arg the type set of Iterable
     * @ret { Set of Iterables. }
     */
    Iterable := T -> (
        // Iterator implementation, which is a subset of Ite
        IteImpl -: @Ite(T),

        // An iterator for the head of this iterable
        IteImpl head,

        /*
         * @arg the iterator in this iterable
         * @ret if the iterator has next position available
         */
        hasNext -: ToBool(IteImpl),

        /*
         * @arg the iterator in this iterable
         * @ret the iterator denoting the next position
         */
        next -: Self(IteImpl)
    );

    // Loops through the iterable.
    Loop := (V, T) -> (
        (Iterable(T) iterable, V initial, Consumer(V, T) consumer) -> For(
            (iterable($head), initial),
            (Ite(T) ite, V val) -> iterable($hasNext)(ite),
            (Ite(T) ite, V val) -> (iterable($next)(ite), consumer(val, ite($value)))
        )
    );
}