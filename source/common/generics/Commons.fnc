func "common.generics.Commons". {
    // From native types
    FromBool,   { V, ?. ret, V, Function (Bool:V). } ret.
    FromInt,    { V, ?. ret, V, Function (Int:V). } ret.
    FromFloat,  { V, ?. ret, V, Function (Float:V). } ret.
    FromChar,   { V, ?. ret, V, Function (Char:V). } ret.

    FromString, { V, ?. ret, V, Function (String:V); } ret;


    // To native types
    ToBool, { V. ?; ret. V. Function (V:Bool); } ret;
    ToInt, { V. ?; ret. V. Function (V:Int); } ret;
    ToFloat, { V. ?; ret. V. Function (V:Float); } ret;
    ToChar, { V. ?; ret. V. Function (V:Char); } ret;

    ToString, { V. ?; ret. V. Function (V:String); } ret;
};