/* 
 * Funcy is a language based on funcy, set-to-set functions.
 * (It has LGPL v3.0 license)
 */

import "lang.format.Import"
import ~ {
    import None := import {
        "lang.format.Format",
        "lang.generics.Generics",
        "lang.system.System",
        "lang.io.InOut",
        "lang.io.Console",
        "lang.memory.Memory",
        "lang.collection.Ites",
        "lang.collection.Array"
    }

    console := Console(System);
    populated := AsArray(1, 2, 3)(GetStack(System));
} -> Loop(populated($output), console, Print(ItoS)) != INVALID    // Compiler deal with guessing the type parameters. Also, lambdas