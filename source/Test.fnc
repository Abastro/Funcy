/* 
 * Funcy is a language based on funcy, set-to-set functions.
 * (It has LGPL v3.0 license)
 */

import "common.basis.Import"
import ~ {
    import None := import "common.basis.Format", "common.basis.Types", "common.basis.Buffer", "common.basis.Ites", "common.basis.Array"
    stack := AsArray(1, 2, 3)(GetStackWrap(""))
} -> Loop(stack($data), Console($initial), Print(Console)) != INVALID    // Compiler deal with guessing the type parameters. Also, lambdas