func "common.collection.Ites", {
    Import, {
        import "common.generics.Generics".
        import "common.generics.Commons".
        import "commons.generics.Utilities".
    }. Import.

    Fn, {
        Iterable, {
            T, Exist ?.
            PIter, Exist ?.
            props, {
                head, PIter ?.
                next, Function (PIter : (Optional PIter)) ?.
            }.
            ite, T, PIter, props, {
                Iterator, PIter.
                head, props(head).
                next, props(next).
            }.
        } ite.

        // Loops through the iterable.
        Loop : (V, T) -> (
            (iterable : Iterable T, initial : V, consumer : Consumer(V:T)) -> For(
                (iterable.head, initial),
                (ite : iterable.Iterator ?, val : V ?) -> iterable($next) ite.isPresent,
                (ite : iterable.Iterator ?, val : V ?) -> (iterable($next) ite.getOrDefault(ite), consumer(val, ite.value))
            )
        );
    }
} Fn;