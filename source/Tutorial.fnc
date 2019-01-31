// Tutorials
{
    .Type = |`Type`|;

    {->} : |`Basis.DepType`| = \dep. \deriver. |`Basis.FuncType`| deriver;
    {-<} : |`Basis.DepType`| = \dep. \deriver. |`Basis.PairType`| deriver;

    .Either : Type -> Type -> Type
        = \left. \right. PairType { .Left = left; .Right = right; };
    .Both : Type -> Type -> Type
        = \left. \right. (left -< right);

    DefaultOf : Type -> Type
        = \D. FuncType { .DefIns = D; };
    {[]} : {[DefaultOf D]} -> D
        = \dof. dof.DefIns

    DecT : Type = PairType {
        .Exist = a;
        .None = a -> Void;
    }

    // Enumeration type
    .Bool = { .TRUE; .FALSE; };
    .True : Bool = .TRUE;
    .False : Bool = .FALSE;

    // Pair Decomposition in lambda
    .SelFrom : Product T T -> Bool -> T
        = \(l, r). { .TRUE = l; .FALSE = r; };

    .Choose : Bool -> Product T T -> T
        = { .TRUE = \(l, r). l; .FALSE = \(l, r). r; };

    .NOT = SelFrom [False, True];
    .XOR = left. SelFrom [NOT left, left];
    .NXOR = left. right. NOT (XOR left right);


    .Always = SelFrom [(), Void];

    // Equivalence
    .Equivalence : Type -> Type
        = \T. (T -> T -> Bool) -< \eq. FuncType {
            .reflective = T -> \x. Always(eq x x);
            .symmetric = T -> \x. T -> \y. Always (eq x y) -> Always(eq y x);
            .transitive = T -> \x. T -> \y. T -> \z. Always (eq x y) -> Always (eq y z) -> Always (eq x z);
        };

    // This instance is necessary, thus default
    #default : (Equivalence Bool) = [ NXOR, {
        .reflective = { .TRUE = Always (NXOR TRUE TRUE); .FALSE = Always (NXOR FALSE FALSE); };
    }];

    // Using syntax
    // using default
 
    {=} : {[Equivalence T]} -> (T -> T -> Bool)
        = \(eq, verif). eq;

    .Nat = PairType {
        .Z = ();
        .S = Nat;
    };

    .Zero : Nat = [.Z, ];
    .Succ : Nat -> Nat = \n. [.S, n];

    .List : Type -> Type = \T. PairType {
        .Empty = ();
        .More = T -< List;
    }
}
