// Tutorials
{
    Void = Form {},
    Single = Form {
        ID = Derive {}
    },

    Bool = Form {
        TRUE = Derive (T ~> left : T -> right : T -> left),
        FALSE = Derive (T ~> left : T -> right : T -> right)
    },
    TRUE = Bool $TRUE,
    FALSE = Bool $FALSE,

    NOT = flag : Bool -> flag FALSE TRUE,
    XOR = left : Bool -> right : Bool -> left (NOT right) (right)

    Always = flag : Bool -> flag Single Void,

    Equivalence : T => @eq : (T -> T -> Bool) -> #{
        equiv = eq,

        condition = {
            reflective = x : T -> Always (eq x x),
            symmetric = x : T -> y : T -> p : Always (eq x y) -> Always (eq y x),
            transitive = x : T -> y : T -> z : T -> left : Always (eq x y) -> right : Always (eq y z) -> Always (eq x z)
        }
    },
    (==) : T => Equivalence T $equiv,

    Equivalence Bool = {
        equiv = left : Bool -> right : Bool -> NOT (XOR left right),
        condition = equiv 
    }

    //flag : Bool -> T ~> left : (Assert flag -> T) -> right : T -> flag (left (Single $ID)) right

    Sum = first : Form -> second : Form -> Form {
        First = x : first -> Derive x,
        Second = x : second -> Derive x
    },

    Product = T : Form -> S : Form -> Form (
        left : T -> right : S -> Derive { Left = left, Right = right }
    ),

    left : Bool -> right : Bool -> Product Bool Bool left right : PB

    NumImpl = {
        Z = T ~> f : (T -> T) -> x : T -> x,
        S = T ~> n : ((T -> T) -> (T -> T)) -> f : (T -> T) -> x : T -> f (n f x)
    }

    s = $Z -> NumImpl s : Num
    s = $S -> n : Num -> NumImpl s n : Num

    Num = Form {
        Z = Derive (T ~> f : (T -> T) -> x : T -> x),
        S = n : Num -> Derive (T ~> f : T -> T -> x : T -> f (n f x))
    }

    Pre = n : Num -> Expand n { Z = Num $Z , S = m : Num -> m }

    Lists = Form (
        len : Num -> x : List len -> Derive x
    )

    Length = l : Lists -> Expand l (len : Num -> len)
}