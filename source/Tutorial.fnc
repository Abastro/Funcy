// Tutorials
{
    .Type = `Type`;
    .DeriverFrom = T. ({{ `Basis.TypeToType` }} x. Type);

    .( ) :  = (inType. outType.) func. input. (_apply (_pair func input));

    .(->) = dep. deriver. ForAll ({{DeriverFrom dep}} deriver);
    .(-<) = dep. deriver. ForOne ({{DeriverFrom dep}} deriver);

    .Sum = first. second. ForOne { .First = first; .Second = second; };
    .Product = left : Type, right : Type, (left -< right);

    .Bool = { .TRUE, .FALSE };
    .True : Bool = .TRUE
    .False : Bool = .FALSE

    .SelFrom : Product T T -> Bool -> T
        = (l, r). { .TRUE = l; .FALSE = r; };

    .Choose : Bool -> Product T T -> T
        = { .TRUE = (l, r). l; .FALSE = (l, r). r; };

    .NOT = SelFrom [False, True];
    .XOR = left. SelFrom [NOT left, left];
    .NXOR = left. right. NOT (XOR left right);


    .Always = SelFrom [(), Void];

    // Equivalence
    .Equivalence = Type -> T. (
        (T -> T -> Bool) -< eq. ForAll {
            $reflective = T -> x. Always(eq x x);
            $symmetric = T -> x. T -> y. Always (eq x y) -> p. Always(eq y x);
            $transitive = T -> x. T -> y. T -> z. Always (eq x y) -> left. Always (eq y z) -> right. Always (eq x z);
        };
    )

    x : Bool, NXOR x x;
    { $TRUE = NXOR $TRUE $TRUE; $FALSE = NXOR $FALSE $FALSE; };
    { $TRUE = $TRUE; $FALSE = $TRUE };
    x : Bool, $TRUE;
    x. Always(eq x x) <=> {{ Bool -> Type }} x. ()


    default : (Equivalence Bool) = [ NXOR, {
        .reflective = [f, n] : Bool, { TRUE = [] : Always (NXOR TRUE TRUE); FALSE = [] : Always (NXOR FALSE FALSE) } f;
    }];

    .Nat = ForOne {
        .Z = Unit;
        .S = Nat;
    };

    .Zero : Nat = (.Z, );
    .Succ : Nat -> Nat = n. (.S, n);

    .List = T. ForOne {
        .Empty = Unit;
        .More = T -< List;
    }
}