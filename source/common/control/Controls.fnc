import "lang/format/Import"
import -> export {
    import None := import { "lang/format/Format", "lang/generics/Generics", "lang/generics/Commons" }

    // Conditional Statements

    Either := T -> (T forTrue, T forFalse) -> (FromBool(T)) (
        TRUE : forTrue,
        FALSE : forFalse
    );

    Choose := T -> (Bool flag) -> (
        (T forTrue, T forFalse) -> Either(forTrue, forFalse)(flag)
    );

    // Loop statements

    For := I -> (ToBool(I) condition, Self(I) increase) -> {
        // Looper Declaration
        Self(I) impl;

        // Looper Definition
        impl := I value -> Choose(condition(value)) (
            impl(increase(value)),
            value
        );
    } ~ impl;

    For := (C, I) -> (
        (ToBool(State(C)(I)) condition, Consumer(I)(C) consumer) -> (Self(State(C)(I))) (
            State(C)(I) state -> For(condition, WrapT(consumer))(state)
        )
    );
}