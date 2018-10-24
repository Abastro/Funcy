"Generics" in "common.generics"

// NOTE: Now going back to functional language
// Pair - a function with a single input and a single output
{
    Type = \F : \typed = F ? : typed;
    TypeD = \F : \typed = F ? ? : typed;

    InOf = \pair = (?:?) : Con (Domain pair);
    OutOf = \pair = (?:?) : Con pair;
    hid NPair = \TypePair = (?:?) : \inp = InOf TypePair ? : \outp = OutOf TypePair ? : (inp : outp);
    Pair = \TypePair = (?:?) : TypeD (NPair TypePair);

    TRUE = InOf;
    FALSE = OutOf;
    Bool = { TRUE : TRUE; FALSE : FALSE; };

    Self = \T : Function (T:T); // This takes expression as a parameter

    // Identity
    Id = \T : Self T (\v = T ? : v);

    // Null
    Void = Type ({} : {});
    NullOr = \T : {Type T; Void;};

    NonNull = Type ( \F : F );
    Nullable = NullOr NonNull;
 
    // Consumer and Supplier
    Consumer = \V : \I : Function ( Pair(V:I) : V );
    Supplier = \V : \O : Function ( V : Pair(V:O) );

    // Composition
    hid CompType = \TypePair = (?:?) : \M = Nullable ? : ?;
    hid CompType = \TypePair = (?:?) : {
        \M : (
            Function (InOf TypePair : M) : TypeD (CompType (M : OutOf TypePair))
        );
        {} : (
            Function TypePair : Void
        )
    };

    Comp = \TypePair = (?:?) : \fns = CompType ? TypePair ? : Function TypePair ?;
    Comp = \TypePair = (?:?) : (
        \fns = CompType {} TypePair ? : InOf fns |
        \fns = CompType (NonNull ?) TypePair ? : Function TypePair ( \param : InOf TypePair ? = (OutOf fns) (InOf fns param) )
    );
}