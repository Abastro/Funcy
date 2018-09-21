"Ites" in "common.collection"
include "lang.format.Import"

import {"common.generics.Generics", "common.generics.Commons", "commons.generics.Utilities"} ~ {
    /*
     * @arg the type set of Iterator
     * @ret { Set of Iterators. Denotes certain position in the Iterable. }
     */
    Ite := T -> T value;

    /*
     * @arg the type set of Iterable
     * @ret { Set of Iterables. }
     */
    Iterable := T -> {
        // Iterator implementation
        IteImpl -= @Ite(T);
    } ~ (
        // The implementation is exposed
        Impl : IteImpl,

        // The iterator head is exposed
        IteImpl head,

        /*
         * @arg the iterator in this iterable
         * @ret the optional for iterator denoting the next position, or empty set if it doesn't exist
         */
        next => (IteImpl ~> Optional(IteImpl))
    );

    // Loops through the iterable.
    Loop := (V, T) -> (
        (Iterable(T) iterable, V initial, Consumer(V, T) consumer) -> For(
            (iterable($head), initial),
            (Ite(T) ite, V val) -> iterable($next) (ite)($isPresent),
            (Ite(T) ite, V val) -> (iterable($next) (ite)($getOrDefault) (ite), consumer(val, ite($value)))
        )
    );
}