"Ites" in "common.collection"
include "lang.format.Import"

import {
    "common.generics.Generics",
    "common.generics.Commons",
    "commons.generics.Utilities"
} ~ {
    Iterable := T -> (
        PIter -> props : func {
            head : PIter ?;
            next : Function PIter (Optional PIter) ?;
        } -> {
            Iterator : PIter;
            head : props.head;
            next : props.next;
        }
    );

    // Loops through the iterable.
    Loop := (V, T) -> (
        (iterable :: Iterable T, initial :: V, consumer :: Consumer(V:T)) -> For(
            (iterable.head, initial),
            (Ite(T) ite, V val) -> iterable($next) ite.isPresent,
            (Ite(T) ite, V val) -> (iterable($next) ite.getOrDefault(ite), consumer(val, ite.value))
        )
    );
}