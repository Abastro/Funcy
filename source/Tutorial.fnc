// Tutorials
{
    Void = Sigma {};
    Unit;

    // Mapping Form

    Sum = first : Form, second : Form, { First =< first; Second =< second; }
    Product = left : Form, right : Form, (left -< right);

    Bool = { TRUE =< Unit; FALSE =< Unit; };
    TRUE = [$TRUE, Null] : Bool,
    FALSE = [$FALSE, Null] : Bool,

    NOT = [f, n] : Bool, { TRUE = FALSE; FALSE = TRUE; } f;
    XOR = left : Bool, [f, n] : Bool, { TRUE = NOT left; FALSE = left; } f;

    Equivalence : T ->> (eq : T -> T -> Bool) -< {
        reflective => x : T -> Always (eq x x),
        symmetric => x : T -> y : T -> p : Always (eq x y) -> Always (eq y x),
        transitive => x : T -> y : T -> z : T -> left : Always (eq x y) -> right : Always (eq y z) -> Always (eq x z)
    },

    Equivalence Bool = [ left : Bool -> right : Bool -> NOT (XOR left right), ]

    Num = Sigma {
        Z = Unit;
        S = Num;
    };
    Z = [$Z, Null];

    Pre = [r, n] : Num -> { Z = Z, S = n } r;

    List = T : Form -> Sigma (
        len : Num -> Vec T len
    );

    Length = [len, vec] : List -> len;
}