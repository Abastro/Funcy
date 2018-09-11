// Code -> Token -> AST

funcSyntax ~ {
    // Being empty / nonempty is heavily optimized
    SingletonSet := singleton | (x | singleton == {x}) != {};

    ComplementEval := complementPair | (
        arg ~ {
            ret := element | (x -= arg | x == element) != {}
        } | complementPair == {{arg}, {arg, ret}}
    ) != {}

    UnionEval := unionPair | (
        arg ~ {
            ret := element | (
                set -= arg | (x -= set | x == element) != {}
            ) != {};
        } | unionPair == {{arg}, {arg, ret}} // This pair is also optimized
    ) != {};

    IntersectionEval := intersectionPair | (
        arg ~ {
            ret := element ~ {
                containing := (set -= arg | (x -= set | x == element));
                notContaining := (comp -= arg | (set -= containing | set == comp) == {})
            } | notContaining == {};
        } | intersectionPair == {{arg}, {arg, ret}}
    )

    ResultSet := {
        
    }

} | (x -= ResultSet | x == funcSyntax) != {}