"Generics" in "common.generics" {
    Type. { F. ?; typed. F ?; } typed;
    TypeD. { F. ?; typed. F ? ?; } typed;

    InOf. \pair = (?:?) : Con (Domain pair);
    OutOf. \pair = (?:?) : Con pair;
    hid NPair. \TypePair = (?:?). \inp = InOf TypePair ?. \outp = OutOf TypePair ?. (inp : outp);
    Pair. \TypePair = (?:?). TypeD (NPair TypePair);

    TRUE. InOf;
    FALSE. OutOf;
    Bool. { TRUE. TRUE; FALSE. FALSE; };


    Self. { T. ?; ret. Function (T:T); } ret;

    // Identity
    Id. { T. ?; ret. Self T ( {v. T ?;} v ); } ret;

    // Null
    Void. Type ({}. {});
    NullOr. { T. ?; ret = {Type T; Void;} } ret;

    NonNull. Type ({ F. ?; } F);
    Nullable. NullOr NonNull;
 
    // Consumer and Supplier
    Consumer. { V. ?; I. ?; ret. Function ( Pair(V:I) : V ); } ret;
    Supplier. \V. \O. Function ( V : Pair(V:O) );

    // Composition
    hid CompType. \TypePair = (?:?). \M = Nullable ?. ?;
    hid CompType. \TypePair = (?:?). {
        \M. (
            Function (InOf TypePair : M) : TypeD (CompType (M : OutOf TypePair))
        );
        {}. (
            Function TypePair : Void
        )
    };

    Comp. \TypePair = (?:?). \fns = CompType ? TypePair ?. Function TypePair ?;
    Comp. \TypePair = (?:?). (
        \fns = CompType {} TypePair ?. InOf fns |
        \fns = CompType (NonNull ?) TypePair ?. Function TypePair ( \param : InOf TypePair ? = (OutOf fns) (InOf fns param) )
    );
};