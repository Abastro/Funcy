// Tutorials
// Needs more change
{
    Bool = {
        TRUE = { T : Type ~> left : T -> right : T -> return = left },
        FALSE = { T : Type ~> left : T -> right : T -> return = right }
    } ?,

    Sum = {
        first : Type -> second : Type -> {
            First = { x : first -> return = x },
            Second = { x : second -> return = x }
        } ? ?
    },

    Product = {
        T : Type -> S : Type -> {
            left : T, right : S -> Left = left, Right = right
        } ?
    },

    ChurchNum = {
        Z = { T : Type ~> f : { ? : T -> ? : T } -> x : T -> return = x },
        S = { n : ChurchNum -> T : Type ~> f : { ? : T -> ? : T } -> x : T -> return : f (n f x) } ?
    } ?,

    ChurchNum S (ChurchNum Z)
}