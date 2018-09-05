/* 
 * Funcy is a language based on funcy, set-to-set functions.
 * (It has LGPL v3.0 license)
 */

import "common.basis.Import"
import ~ {
    import None := import "common.basis.Format", "common.basis.Types", "common.basis.Buffer", "common.basis.Ites", "common.basis.Array"
} -> Loop(AsArray(1, 2, 3), Console($initial), Print(Console)) != INVALID    // Compiler deal with guessing the type parameters. Also, lambdas