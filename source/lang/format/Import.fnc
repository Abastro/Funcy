// Code -> Token -> AST

{
    SingletonSet := singleton | (x | singleton == {x}) != {};

    IndexedCharSet := indexed | (
        index -= IntSet | (
            char -= CharSet | indexed == {index, char}
        ) != {}
    ) != {};

    //StringSet := string | (ic -= string | ic -= IndexedCharSet) == string;

} ~ syntax {
    internal {
        "state",
        {
            "\"((\\\\)\"|[^\\n\"])*\"",
            {"literalString"}
        }
    }

    { // Function substitution
        3,
        subPair | (
            prev -= SetDescSet ~ {

            } | subPair == {{prev}, {prev, next}}
        ) != {}
    },

    // Interpretation
    {
        1,
        FunctionPair ~ {

        } | (
            ( FunctionSyntax |
                ( FunctionCapture |
                    (
                        FunctionPair == {{FunctionSyntax}, {FunctionSyntax, FunctionCapture}} /\
                        FunctionSyntax -= parse(".*->.*")
                    )
                ) != {} 
            ) != {}
        )
    }

}
(x, y) -> {{x}, {x, y}}


// Basis for everything, because this deals with import statements.
{
    Reference := {TRUE} -> #String;

    // Easy reference creation method
    template EasyRef := syn -= CodeSet(Funcy) -> (
        {FALSE} -> syn,
        {TRUE} ~ {
            refName := AsName(syn);
            name := SplitLast("$")(syn);
        } -> (
            {FALSE} -> DeclareIfAbsent(refName)(({TRUE} -> name) inherits Reference),
            {TRUE} -> DeclaredFuncy(refName)
        ) inherits CodePair
    )(Match("$\p{word}")(syn));


    // Import statement
    template ImportDecl := syn -= CodePairSet({FALSE} -> Ident, {TRUE} -> func) ~ {
        ident := syn(FALSE);
        flag := Include(" import ")(ident);
    } -> (
        
    );

    // Export statement
    template Export := (
        
    );
} -> (
    $Reference -> Reference,
    $EasyReference -> EasyReference
)