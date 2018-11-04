// Tutorials
{
    Void = @{},

    Single = @{
        ID = #(T ~> x : T -> x)
    },

    Bool = @{
        TRUE = #(T ~> left : T -> right : T -> left),
        FALSE = #(T ~> left : T -> right : T -> right)
    },
    TRUE = Bool $TRUE,
    FALSE = Bool $FALSE,

    Inv = flag : Bool -> flag FALSE TRUE,
    XOR = left : Bool -> right : Bool -> left (Inv right) (right)

    Assert = flag : Bool -> flag Single Void,

    Equivalence : T => eq : (T -> T -> Bool) -> {
        equiv = eq,

        condition = {
            reflective = x : T -> Assert (eq x x),
            symmetric = x : T -> y : T -> p : Assert (eq x y) -> Assert (eq y x),
            transitive = x : T -> y : T -> z : T -> left : Assert (eq x y) -> right : Assert (eq y z) -> Assert (eq x z)
        }
    },
    (==) : T => Equivalence T $equiv,

    Equivalence Bool = {
        equiv = left : Bool -> right : Bool -> Inv (XOR left right),
        condition = equiv 
    }

    //flag : Bool -> T ~> left : (Assert flag -> T) -> right : T -> flag (left (Single $ID)) right

    Sum = {
        first : Form -> second : Form -> @{
            First = x : first -> #x,
            Second = x : second -> #x
        }
    },

    Product = {
        T : Form -> S : Form -> @(
            left : T -> right : S -> #{ Left = left, Right = right }
        )
    },

    Num = @{
        Z = #(T ~> f : (T -> T) -> x : T -> x),
        S = n : ? -> #(T ~> f : T -> T -> x : T -> f (n f x))
    },

    Num $S (Num $Z)
}