import "lang/format/Import"
import -> export {
    import None := import "lang/format/Format", "lang/generics/Generics", "lang/generics/Commons"

    // Conditional Statements
    Either := T -> (T $forTrue, T $forFalse) -> (
        TRUE : $forTrue,
        FALSE : $forFalse
    ) -= FromBool(T);

    Choose := T -> (Bool flag) -> (
        (T $forTrue, T $forFalse) -> Either($forTrue, $forFalse)(flag)
    );

    // Loop statements
    For := I -> (ToBool(I) condition, Self(I) increase) ~ {
        // Declaration
        Self(I) impl;

        // Implementation
        impl := I value -> Choose(condition(value)) (
            impl(increase(value)),
            value
        );
    } -> impl;

    For := (C, I) -> (
        (ToBool(State(C)(I)) condition, Consumer(I)(C) consumer) -> (
            State(C)(I) state -> For(
                condition, WrapT(consumer)
            )(state) -= Self(State(C)(I))
        )
    );
}