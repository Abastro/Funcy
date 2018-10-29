func "common.collection.Ites", {
    $Import, {
        import "common.generics.Generics".
        import "common.generics.Commons".
        import "commons.generics.Utilities".
    }. Import.

    $Fn, {
        $head, PIter ?.
        $next, Function (PIter : NullOr PIter) ?.
        $propForm, pHead, pNext, { $head, head. $next, next. }.

        $Iterable, {
            $T, Exist ?.
            $PIter, Exist ?.
            $props, propForm ? ?.
            $ite, T, PIter, props, {
                $Iterator, PIter.
                $head, props $head.
                $next, props $next.
            }.
        } $ite.

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