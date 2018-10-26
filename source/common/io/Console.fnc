func "common.io.Console", {
    Import, {
        import "common.io.InOut".
        import "common.system.System".
    }. Import.

    Fn, {
        Console, System, OutStream ?.
        import "internal.io.Console".
    }.
} Fn;