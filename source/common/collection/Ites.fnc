"Ites" in "common.collection"
include "lang.format.Import"

import {
    "common.generics.Generics",
    "common.generics.Commons",
    "commons.generics.Utilities"
} ~ {
    AsIterable := T -> (
        PIter -> props :: {
            head :: PIter;
            next :: (PIter ~> Optional(PIter));
        } -> {
            Iterator : PIter,
            head : props.head,
            next : props.next
        }
    );

    Iterable := T -> Image(IterableFor(T)(?));

    // Loops through the iterable.
    Loop := (V, T) -> (
        (Iterable(T) iterable, V initial, Consumer(V, T) consumer) -> For(
            (iterable.head, initial),
            (Ite(T) ite, V val) -> iterable($next) ite.isPresent,
            (Ite(T) ite, V val) -> (iterable($next) ite.getOrDefault(ite), consumer(val, ite.value))
        )
    );
}