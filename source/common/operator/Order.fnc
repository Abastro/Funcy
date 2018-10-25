in "common.order.Order" {
    hid import "common.generics.Generics";
    hid import "common.generics.Commons";

    Ordering. {
        T. ?;
        param. {
            lessThan. Function (T:T) ?;
            moreThan. Function (T:T) ?;
            lessEqThan. Function (T:T) ?;
            moreEqThan. Function (T:T) ?;
        };
        ret. T. param. param;
    } ret;
}