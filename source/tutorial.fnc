// Tutorials
// Block construct narrows down objects - how?
{
    $Bool, {
        $TRUE : Any, Any, Any.
        $TRUE, x, y, x.

        $FALSE : Any, Any, Any.
        $FALSE, x, y, y.
    } ?.

    $Sum : Type, Type, Type.
    $Sum, T, S, { $left, T. $right, S. } ? ?.

    $Sum Int Bool $left 3

    $Product : Type, Type, Type.
    $Product, T, S, ( x : T, y : S, { $left, x. $right, y. } ) ? ?.
}