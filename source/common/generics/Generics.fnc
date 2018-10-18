"Generics" in "common.generics"

// NOTE: Now going back to functional language
// Pair - a function with a single input and a single output
{
    Type : F -> ( typed : F ? -> typed );
    TypeD : F -> ( typed : F ? ? -> typed );

    InP : ( pair : (?:?) ) -> Con (Domain pair);
    OutP : ( pair : (?:?) ) -> Con pair;
    hid NTypeP : ( TypePair : (?:?) ) -> inp : InP TypePair ? -> outp : OutP TypePair ? -> inp : outp;
    TypeP : ( TypePair : (?:?) ) -> TypeD (NTypeP TypePair);

    Self : T -> Function (T:T); // This takes expression as a parameter

    // Identity
    Id : T -> Self T (v : T ? -> v);

    // Null
    Void : Type ({} : {});
    Nullable : Type ( F -> F | {} : {} );

    // Consumer and Supplier
    Consumer : V -> I -> Function ( TypeP(V:I) : V );
    Supplier : V -> O -> Function ( V : TypeP(V:O) );

    // Composition
    hid CompositeType : ( TypePair : (?:?) ) -> (M : Nullable ?) -> ?;
    hid CompositeType : ( TypePair : (?:?) ) -> (
        M -> (
            Function (InP TypePair : M) : TypeD (CompositeType (M : OutP TypePair))
        ) | {} : (
            Function (InP TypePair : OutP TypePair) : Void
        )
    );
    hid CompType : ( TypePair : (?:?) ) -> TypeD (CompositeType TypePair)
    Comp : ( TypePair : (?:?) ) -> ( fns : CompType TypePair ? ) -> ?
    Comp : ( TypePair : (?:?) ) -> ( fns : CompositeType {} ?
}