import "#common.basis.Import"
import -> export {
    import None := import "common.basis.Format"

    Self    := [V] &V -> &V
    Id      := [V] (V v -> v) inherits Self(V)      // Function definition. Generics are simply function calls
    FromBool := [V] &Bool -> &V
    FromInt := [V] &Int -> &V
    ToBool  := [V] &V -> &Bool
    ToInt := [V] &V -> &Int

    For -= [F] (F initial, ToBool(F) condition, Self(F) increase) -> &F

    toString := [V] V -> % String

    Consumer := [V, I] (V value, I input) -> %C // Maps to nothing here - an interface is first shown here. Native means it's fullfiled

    Pair := [T, S] (T left, S right)     // Interface. Could be a type as well
}