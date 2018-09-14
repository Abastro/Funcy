import "lang/format/Import"
import -> export {
    import None := import "lang/format/Format", "lang/generics/Generics"

    // From native types
    FromBool := V -> Transform(Bool, V)
    FromInt := V -> Transform(Int, V);
    FromFloat := V -> Transform(Float, V);
    FromChar := V -> Transform(Char, V);

    FromString := V -> Transform(String, V);


    // To native types
    ToBool  := V -> Transform(V, Bool);
    ToInt := V -> Transform(V, Int);
    ToFloat := V -> Transform(V, Float);
    ToChar := V -> Transform(V, Char);

    ToString := V -> Transform(V, String);
}