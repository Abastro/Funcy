in "common.generics.Generics" {
    Type. { F. ?; typed. F ?; ret. F. typed. typed; } ret;
    TypeD. { F. ?; typed. F ? ?; ret. F. typed. typed; } ret;

    InOf. \pair = (?:?) : Con (Domain pair);
    OutOf. \pair = (?:?) : Con pair;
    hid NPair. \TypePair = (?:?). \inp = InOf TypePair ?. \outp = OutOf TypePair ?. (inp : outp);
    Pair. \TypePair = (?:?). TypeD (NPair TypePair);

    TRUE. InOf;
    FALSE. OutOf;
    Bool. { TRUE. TRUE; FALSE. FALSE; };


    Self. { T. ?; ret. T. Function (T:T); } ret;

    // Identity
    Id. { T. ?; ret. T. Self T ( {v. T ?;} v ); } ret;

    // Null
    Void. Type ({}. {});
    NullOr. { T. ?; ret. {Type T; Void;} } ret;

    NonNull. Type ({ F. ?; ret. F. F } ret);
    Nullable. NullOr NonNull;

    // Consumer and Supplier
    Consumer. { V. ?; I. ?; ret. Function ( Pair(V:I) : V ); } ret;
    Supplier. { V. ?; I. ?; ret. V. I. Function ( V : Pair(V:O) ); } ret;

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