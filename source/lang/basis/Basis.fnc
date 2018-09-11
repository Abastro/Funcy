"Basis" in "lang.basis"

// Just the code which represents the basis module.
basisSyntax ~ {
    ResultSet := {
        { "token",
            {"(\\s|\\n)+", {"ignored"}},

            {
                "\\p{L}+",
                {"ID", {"Identifier"}}
            },
            {
                "[^.][1-9][0-9]*[^.]",
                {"IL", {"Integer Literal"}}
            },
            {
                "(|0|[1-9][0-9]*)\\\\.(|[0-9]*[1-9])",
                {"FPL", {"Floating Point Literal"}}
            },
            {
                "'[^\\\\]|\\\\.'",
                {"CL", {"Character Literal"}}
            },
            {
                "\"[^\"]*\"",
                {"SL", {"String Literal"}}
            },

            { "\\(", {"\\(", {"Open Parenthesis"}} },
            { "\\)", {"\\)", {"Close Parenthesis"}} },
            { "{", {"{", {"Open Brace"}} },
            { "}", {"}", {"Close Brace"}} },
            { "\\[", {"[", {"Open Bracket"}} },
            { "\\]", {"]", {"Close Bracket"}} },

            { ",", {",", {"Comma"}} },

            {"==", {"EQN", {"Equality"}}},
            {"!=", {"IEQ", {"Inequality"}}}
        },

        { "clause",
            {"SET", {"Set"}},
            {"DECL", {"Declaration"}},
            {"DEF", {"Definition"}},
            {"BLK", {"Block"}},
            {"PRDC", {"Predicate"}}
        },

        { "syntax",
            { "SET",
                {
                    {"(ID)", {"REF"}},
                    {"{}", {"EMP"}}
                    {"{(SET)(,(SET))*}", {"GRP"}}
                }
            }
        }
    }
} | (x -= ResultSet | x == basisSyntax) != {}