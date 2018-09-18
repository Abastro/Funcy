// Basis for everything, because this deals with import statements.
// TODO Assertions
"Import" in "lang.format"
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