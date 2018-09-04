/* 
 * Funcy is a language based on funcy, set-to-set functions.
 * (It has LGPL v3.0 license)
 */

{
    Inclusions := (
        lib(#common.basis.Import),
        lib(#common.basis.Format),
        lib(#common.basis.Types),
        lib(#common.collection.Ites),
        lib(#common.collection.Array),
        lib(#common.buffer.Buffer)
    )
}
(inc -= &Inclusions) ~ {
    import inc(#common.basis.Import),
    import inc(#common.basis.Format),
    import inc(#common.basis.Types)

    AsArray = [F] (F a1, F a2, F a3) ~ {
        pointer := NewPointer(F)((p1 : a1, p2 : a2, p3 : a3), 3);
    } -> (Array) (3, -1, pointer)
} -> Loop(AsArray(1, 2, 3), Console($initial), Print(Console)) != INVALID    // Compiler deal with guessing the type parameters. Also, lambdas