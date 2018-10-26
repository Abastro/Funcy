func "common.generics.Generics", {
    Nulls, {
        Void, {}, {}.
        NullOr, { T, ?. predicate, ~(T $ {}). ret, T, { T. Void. }. } ret.

        Exist, { value, ?. predicate, ~( ({}. {}) $ value ). ret, value, value. } ret.
    }.

    Boolean, {
        TRUE, {
            first, ?. second, ?.
            ret, (first, second, first).
        } ret.
        FALSE, {
            first, ?. second, ?.
            ret, (first, second, second).
        } ret.
        Bool, { TRUE, TRUE. FALSE, FALSE. }.
    }.

    Multiple, {
        Boolean.

        @:@, {
            left, ?.
            right, ?.
            selector, Bool ?.

            ret, left, right, selector, (selector left right).
        } ret.

        [], {}.
        [@.@], {
            first, ?.
            remaining, ?.

            ret, first, remaining, (first : remaining).
        } ret.

        Left, { pair, ?:?. ret, pair, pair TRUE. } ret.
        Right, { pair, ?:?. ret, pair, pair FALSE. } ret.
    }.

    Type, { F, Exist ?. typed, F ?. ret, F, typed, typed. } ret.

    Function. { x. (Exist ?) : (Exist ?); fn. (Left x ?. Right x ?); ret. fn. fn; } ret;

    Functions, {
        Multiple.
        Self, { T, Exist ?. ret, T, Function (T:T). } ret.

        // Identity
        Id, {
            T, ?.
            ret, T, Self T ( {v, T ?. fn, v, v. } fn ).
        } ret.

        // Consumer and Supplier
        Consumer, { V, ?. I, ?. ret, V, I, Function ( Pair(V:I) : V ). } ret.
        Supplier, { V, ?. O, ?. ret, V, O, Function ( V : Pair(V:O) ). } ret.
    }.

    HiddenComp, {
        Nulls.

        // Composition
        CompType. \TypePair = (?:?). \M = Nullable ?. ?;
        CompType. \TypePair = (?:?). {
            { M. Exist ?; ret. M. Function (InOf TypePair : M) : TypeD (CompType (M : OutOf TypePair)); } ret;
            {}. Function TypePair : Void;
        };
    }.

    HiddenComp.

    Composition. {
        Comp. \TypePair = (?:?). \fns = CompType ? TypePair ?. Function TypePair ?;
        Comp. \TypePair = (?:?). {
            \fns = CompType {} TypePair ?. Left fns;
            \fns = CompType (Exist ?) TypePair ?. Function TypePair ( \param : Left TypePair ? = (Right fns) (Left fns param) )
        );
    };

    Fn, {
        Nulls.
        Boolean.
        Multiple.
        Functions.
        Composition.
    }.
} Fn;