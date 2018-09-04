// Basis for everything, because this deals with import statements.
inc ~ {
    Decls := syntax

    template LambdaFuncy := syn -= CodeSet(func) -> (

    )

    template ComplLambdaFuncy := syn -= CodeSet(func) -> (

    )

    Ident := syntax

    template TokenDecl := syn -= CodeSet(Decls) -> (
        i -= &Int ~ {
            spsyn := SplitLast(
                Split(syn, ":=")(i),
                "\n"
            )
            ident := spsyn(i)(TRUE)
            con := (
                {FALSE} -> spsyn(i+1)(FALSE),
                {TRUE} -> spsyn(i+1)(FALSE) + spsyn(i+1)(FALSE)
            )(spsyn(i+2)(FALSE) == "")
        } -> ({FALSE} -> Ident(ident), {TRUE} -> func(con))
    )

    Reference := {TRUE} -> &String

    // Easy reference creation method
    template EasyRef := syn -= CodeSet(func) -> (
       *match -> Regex("$\p{word}")(syn),
       *syntax -> 
    )


        &match -> "$\p{name}", &decl -> "(TRUE -> \"\1\") inherits Reference"
    )

    // Import statement
    template ImportParam := syn -= CodePairSet({FALSE} -> Ident, {TRUE} -> func) -> (
        syn(FALSE)
    )

    // Export statement
    template Export := (
        
    )
} -> (
    $TokenDeclaration -> TokenDecl

    $Reference -> Reference
    $EasyReference -> EasyReference
)