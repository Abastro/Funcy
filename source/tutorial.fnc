// Tutorials
{
    Func0, I32 5.

    Func1, { switch, Bool ?. result, switch, !switch } result.

    ThisIsFalse, Func1 TRUE.

    Func2, { { i, I32 ?. result, i, i+1. } result. thisIsZero, Func1 FALSE. }.

    ThisIsTwo, TF 1.
    ThisIsZero, TF thisIsZero.
}