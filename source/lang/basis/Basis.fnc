"Basis" in "lang.basis"

// Code -> Token -> AST
// Just the code which represents the basis module.
{ basisSyntax ||
    ResultSet -= {{
        "token" : {
            "(\\s|\\n)+" : "ignored",

            "\\p{L}+" : ("ID" : "Identifier"),
            "'[^\\\\]|\\\\.'" : ("CL" : "Character Literal"),
            "\"[^\"]*\"" : ("SL" : "String Literal"),

            "\\(" : ("\\(" : "Open Parenthesis"),
            "\\)" : ("\\)" : "Close Parenthesis"),
            "{" : ("{" : "Open Brace"),
            "}" : ("}" : "Close Brace"),

            "," : ("," : "Comma"),

            "==" : ("EQN" : "Equality"),
            "!=" : ("IEQ" : "Inequality")
        },

        "clause" : {
            "SET" : "Set",
            "DECL" : "Declaration",
            "PRDC" : "Predicate"
        },

        "syntax" : {
            "SET" : {
                {
                    "(ID)" : "REF",
                    "{}" : "EMP",
                    "{(SET)(,(SET))*}" : "GRP"
                }
            }
        }
    }} || {x -= ResultSet || x == basisSyntax} != {}
}