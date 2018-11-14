// Tutorials
{
    (=>) = X : Type ' Y : Type ' FuncType X Y

    ForOne : (T => Type) => Type;
    ForAll : (T => Type) => Type;

    Pair : SpecifyOn Name T => Clause Name => ForOne Typer;
    Lambda : QualifyOn Name T => Clause Name => ForAll Typer;

    Sum = first : Form, second : Form, ForOne { First = first; Second = second; };
    Product = left : Form, right : Form, (left -< right);

    Bool = ForOne { TRUE = Unit; FALSE = Unit; };
    TRUE = [$TRUE, Null] : Bool,
    FALSE = [$FALSE, Null] : Bool,

    SelFrom = [l, r] : Product T T, [f, n] : Bool, { TRUE = l; FALSE = r; } f;
    Choose = [f, n] : Bool, { TRUE = [l, r] : Product T T, l; FALSE = [l, r] : Product T T, r; } f;

    NOT = SelFrom [FALSE, TRUE];
    XOR = left : Bool, SelFrom [NOT left, left];
    NXOR = left : Bool, right : Bool, NOT (XOR left right);

    Always = SelFrom [Unit, Void];

    Equivalence = (eq : T -> T -> Bool) -< ForAll {
        reflective = x : T -> Always (eq x x);
        symmetric = x : T -> y : T -> p : Always (eq x y) -> Always (eq y x);
        transitive = x : T -> y : T -> z : T -> left : Always (eq x y) -> right : Always (eq y z) -> Always (eq x z);
    };

    x : Bool, NXOR x x;
    [f, n] : Bool, { TRUE = NXOR [f, n] [f, n]; FALSE = NXOR [f, n] [f, n]; } f;
    [f, n] : Bool, { TRUE = NXOR [$TRUE, n] [$TRUE, n] TRUE; FALSE = NXOR [$FALSE, n] [$FALSE, n]; } f;
    [f, n] : Bool, { TRUE = TRUE; FALSE = TRUE; } f;
    x : Bool, TRUE;

    EqBool = [ NXOR, {
        reflective = [f, n] : Bool, { TRUE = Null : Always (NXOR TRUE TRUE); FALSE = Null : Always (NXOR FALSE FALSE) } f;
    }];

    Num = ForOne {
        Z = Unit;
        S = Num;
    };
    Z = [$Z, Null] : Num;
    Succ = n : Num, [$S, n] : Num;

    List = T : Form, len : Num -< Vec T len;

    Length = [len, vec] : List -> len;
}