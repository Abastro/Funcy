/* 
 * Funcy is a language based on funcy, set-to-set functions.
 * (It has LGPL v3.0 license)
 */

{ import None := #common.basis.Import }
(import inc) ~ {
    import None := (
        #common.basis.Import,
        #common.basis.Format,
        #common.basis.Types
    )
} -> Loop(AsArray(1, 2, 3), Console($initial), Print(Console)) != INVALID    // Compiler deal with guessing the type parameters. Also, lambdas