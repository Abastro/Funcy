"Ites" in "common.collection"

{
    import [
        "common.generics.Generics",
        "common.generics.Commons",
        "commons.generics.Utilities"
    ];

    Iterable. {
        T. ?;
        PIter. ?;
        props. {
            head. PIter ?;
            next. Function (PIter : (Optional PIter)) ?;
        };
        ite. {
            Iterator. PIter;
            head. props.head;
            next. props.next;
        }
    } ite;

    // Loops through the iterable.
    Loop : (V, T) -> (
        (iterable : Iterable T, initial : V, consumer : Consumer(V:T)) -> For(
            (iterable.head, initial),
            (ite : iterable.Iterator ?, val : V ?) -> iterable($next) ite.isPresent,
            (ite : iterable.Iterator ?, val : V ?) -> (iterable($next) ite.getOrDefault(ite), consumer(val, ite.value))
        )
    );
}