import "lang/format/Import"
import -> export {
    import None := import "lang/format/Format", "lang/generics/Generics", "lang/generics/Commons"

    // Conditional Statements
    Either := [T] (T forTrue, T forFalse) -> (
        {TRUE} -> forTrue,
        {FALSE} -> forFalse
    ) inherits FromBool(T)

    Choose := [T] (Bool flag) -> (
        (T forTrue, T forFalse) -> Either(forTrue, forFalse)(flag)
    )

    // Loop statements
    For := [I] (ToBool(I) condition, Self(I) increase) ~ {
        // Declaration
        internal -= #Self(I)

        // Implementation
        internal := I value -> Choose(condition(value)) (
            internal(increase(value)),
            value
        )
    } -> internal

    For := [C, I] (ToBool(State(C)(I)) condition, Consumer(I)(C) consumer) -> (
        State(C)(I) state -> For(
            condition, WrapT(consumer)
        )(state) inherits Self(State(C)(I))
    )
}