// Tutorials
// 1. Function
// 2. Block
// 3. Native
// Block construct narrows down objects - how?
{
    $Bool, {
        $TRUE, Any -> Any -> Any # x, y, x.
        $FALSE, Any -> Any -> Any # x, y, y.
    } ?.

    $Sum, Type -> Type -> Type # (
        T, S, { $left, (T -> T # x, x). $right, (S -> S # y, y). } ? ?.
    ).

    $Product, Type -> Type -> Type # (
        T, S, ( T -> S -> Block # x, y, { $left, x. $right, y. } ) ? ?.
    ).
}