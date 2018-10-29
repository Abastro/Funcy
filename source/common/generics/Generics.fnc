func "common.generics.Generics", {
    // Reference-to-Wildcard relations
    // When used, it means 'for any in the image' (should be used after ',' or '{}')
    // When used in param of 'param, return', symbol used in return means the same variable.
    $Nulls, {
        $Void, {}, {}.

        $type, ?.
        // How to deal with predicate?
        $NullOr, type, { predicate, ~(type $ {}). $ret, { type. Void. }. } $ret.
        $type, Void ?.

        $any, ?.
        $Exist, any, { predicate, ~( ({}. {}) $ any ). $ret, any. } $ret.
        $any, Void ?.
    }.

    $Boolean, {
        Nulls.

        $first, ?. $second, ?.
        TRUE, first, second, first.
        FALSE, first, second, second.
        $first, Void ?. $second, Void ?.

        Bool, { TRUE, TRUE. FALSE, FALSE. }.
    }.

    $Multiple, {
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
        First, Left.
        Remaining, Right.
    }.

    //Type, { F, Exist ?. typed, F ?. ret, F, typed, typed. } ret.

    //Function. { x. (Exist ?) : (Exist ?); fn. (Left x ?. Right x ?); ret. fn. fn; } ret;

    Functions, {
        Multiple.

        type, Exist ?.

        Self, type, Function (type:type).

        // Identity
        Id, type, { v, type ?. ret. Self type (v, v) } ret.

        type, Void ?.

        // Consumer and Supplier
        Consumer, { V, ?. I, ?. ret, V, I, Function ( Pair(V:I) : V ). } ret.
        Supplier, { V, ?. O, ?. ret, V, O, Function ( V : Pair(V:O) ). } ret.
    }.

    HiddenComp, {
        Nulls.

        // Composition
        typePair, ?:?.
        CompType, typePair, Exist ?.

        interType, Exist ?.
        CompType, typePair, {
            interType, Function (Left typePair : interType) : TypeD (CompType (interType : Right typePair)).
            {}, Function TypePair : Void.
            interType, Void ?.
        }.
        interType, Void ?.
        typePair, Void ?.
    }. HiddenComp.

    Composition. {
        typePair, ?:?.
        Comp, typePair, \fns = CompType ? TypePair ?. Function TypePair ?;

        initInput, CompType {} typePair ?.
        compInput, CompType (Exist ?) typePair ?.
        Comp, typePair, {
            initInput, Left fns.
            compInput, Function typePair { param, Left typePair ?. ret, param, (Right compInput) (Left compInput param) } ret.
        ).
        typePair, Void ?.
    };

    Fn, {
        Nulls.
        Boolean.
        Multiple.
        Functions.
        Composition.
    }.
} Fn;