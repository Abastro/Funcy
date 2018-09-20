"Sets" in "lang.arithmetics"

funcSyntax || {
    // Being empty / nonempty is heavily optimized
    SingletonSet := { singleton || {x || singleton == {x}} != {} };

    ComplementEval := { complementPair ||
        { arg ||
            { ret -= {{ element || {x -= arg || x == element} == {} }} ||
                complementPair == arg : ret
            } != {}
        } != {}
    };

    UnionEval := { unionPair ||
        { arg ||
            { ret -= {{ element ||
                    { containing -= arg || {x -= containing || x == element} != {} } != {}
                }} || unionPair == arg : ret
            } != {}
        } != {}
    };

    // Equality with the declared set is also optimized
    IntersectionEval := intersectionPair || {
        { arg ||
            { ret -= {{ element ||
                    { containing -= arg || {x -= containing || x == element} != {} } == arg
                }} || intersectionPair == arg : ret
            } != {}
        } != {}
    }
        arg ~ {
            ret := element ~ {
                containing := (set -= arg | (x -= set | x == element));
                notContaining := (comp -= arg | (set -= containing | set == comp) == {})
            } | notContaining == {};
        } | intersectionPair == arg : ret
    )

    ResultSet := {
        
    }

} || (x -= ResultSet | x == funcSyntax) != {}