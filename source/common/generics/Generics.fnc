"Generics" in "common.generics"

// NOTE: Now going back to functional language
// Pair - a function with a single input and a single output
{
    Type : F -> ( typed : F ? -> typed );
    TypeD : F -> ( typed : F ? ? -> typed );

    InP : I -> pair : (I:?) -> Con Domain pair;
    OutP : O -> pair : (?:O) -> Con pair;
    hid NTypeP : ( TypePair : (?:?) ) -> inp : InP TypePair -> outp : OutP TypePair -> inp : outp;
    TypeP : ( TypePair : (?:?) ) -> TypeD (NTypeP TypePair);

    Self : T -> Function T:T; // This takes expression as a parameter

    // Identity
    Id : T -> Self T (v : T -> v);

    // Consumer and Supplier
    Consumer : V -> I -> Function TypeP(V:I) : V;
    Supplier : V -> O -> Function V : TypeP(V:O);
}