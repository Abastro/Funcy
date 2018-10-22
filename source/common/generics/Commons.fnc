"Commons" in "common.generics"

{
    // From native types
    FromBool = \V : Function (Bool:V)
    FromInt = \V : Function (Int:V);
    FromFloat = \V : Function (Float:V);
    FromChar = \V : Function (Char:V);

    FromString = \V : Function (String:V);


    // To native types
    ToBool = \V : Function (V:Bool);
    ToInt = \V : Function (V:Int);
    ToFloat = \V : Function (V:Float);
    ToChar = \V : Function (V:Char);

    ToString = \V : Function (V:String);
}