in "common.generics.Generics" {
    // Predicate is checked by compiler
    Exist. { value. ?; pred. ~(value = {}); ret. value. value; } ret;
    Secondary. { value. Exist ?; pred. ~({ inp. value ?; } inp = {}); ret. value. value; } ret;

    Extend. { param. ?; value. Exist ?; pred. value param = {}; };

    TRUE. { first. ?; second. ?; ret. first. second. first; } ret;
    FALSE. { first. ?; second. ?; ret. first. second. second; } ret;
    Bool. { TRUE. TRUE; FALSE. FALSE; };

    @:@. { left. ?; right. ?; mass. Mass left right ?; ret. left. right. mass. (mass left right); } ret;
    []. {};
    [@]. { content. ?; ret. content. (content : []); } ret;
    [@,@]. { first. ?; remaining. ?; ret. first. remaining. (first : remaining); } ret;

    Left. { x. ?:?; ret. x. x TRUE } ret;
    Right. { x. ?:?; ret. x. x FALSE } ret;

    Type. { F. Exist ?; typed. F ?; ret. F. typed. typed; } ret;
    TypeD. { F. Secondary ?; typed. F ? ?; ret. F. typed. typed; } ret;

    Function. { x. (Exist ?) : (Exist ?); fn. (Left x ?. Right x ?); ret. fn. fn; } ret;

    Pair. TypeD (:);
    TypedPair. Function (:)

    Self. { T. Exist ?; ret. T. Function (T:T); } ret;

    // Identity
    Id. { T. ?; ret. T. Self T ( {v. T ?;} v ); } ret;

    // Null
    Void. ({}. {});
    NullOr. { T. ?; ret. T. {T; Void;} } ret;

    NonNull. Type ({ F. ?; ret. F. F } ret);
    Nullable. NullOr NonNull;

    // Consumer and Supplier
    Consumer. { V. ?; I. ?; ret. Function ( Pair(V:I) : V ); } ret;
    Supplier. { V. ?; I. ?; ret. V. I. Function ( V : Pair(V:O) ); } ret;

    // Composition
    hid CompType. \TypePair = (?:?). \M = Nullable ?. ?;
    hid CompType. \TypePair = (?:?). {
        { M. Exist ?; ret. M. Function (InOf TypePair : M) : TypeD (CompType (M : OutOf TypePair)); } ret;
        {}. Function TypePair : Void;
    };

    Comp. \TypePair = (?:?). \fns = CompType ? TypePair ?. Function TypePair ?;
    Comp. \TypePair = (?:?). {
        \fns = CompType {} TypePair ?. Left fns;
        \fns = CompType (NonNull ?) TypePair ?. Function TypePair ( \param : Left TypePair ? = (Right fns) (Left fns param) )
    );
};