in "common.equal.Equal" {
    hid import "common.generics.Generics";
    hid import "common.generics.Commons";
    Equivalence. {
        T. ?;
        equiv. ToBool (T:T) ?;

        reflexive. { x. T ?; y. T ?; ret. x. y. equiv(x:y) = TRUE. equiv(y:x) = TRUE; } ret;
        transitive. { x. T ?; y. T ?; z. T ?; ret. x. y. z. equiv(x:y) & equiv(y:z) = TRUE. equiv(x:z) = TRUE; } ret;
    }
};