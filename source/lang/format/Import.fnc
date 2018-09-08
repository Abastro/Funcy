// This is the capture structure
("code(?<1>)code", ({1} -> "string"))

{
    
} ~ syntax {
    // simple string to capture structure
    {
        0,
        StringPair ~ {

        } | (
            // String syntax set in the pair
            ( StringSyntax |
                // String capture set in the pair
                ( StringCapture |
                    (
                        StringPair == {{StringSyntax}, {StringSyntax, StringCapture}} /\
                        ..
                    )
                ) != {}
            ) != {}
        )
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
inc -= InternalImports ~ {
    Bool := TotalSet |-> BoolSet
    Int := TotalSet |-> IntSet
    Float := TotalSet |-> FloatSet

    // CodeSet..?
    CodeSet -= Syntax -> NativeSet

    // Simple String Implementation
    String := #Int -> #Char

    // Declarations
    AsString -= #(Union(String)(CodeSet) -> #String)
    DeclareIfAbsent -= #(#String -> #(TRUE -> CodeSet(decl)))
    DeclaredFuncy -= #(#String -> CodeSet(func))


    Funcy -= syntax
    Decls := syntax

    template LambdaFuncy := syn -= CodeSet(Funcy) ~ {
        cond := SplitFirst("->")(syn)(FALSE)
    } -> (

    )

    Ident := syntax

    template TokenDecl := syn -= CodeSet(Decls) -> (
        i -= &Int ~ {
            spsyn := SplitLast(
                Split(":=")(syn)(i),
                "\n"
            )
            ident := spsyn(i)(TRUE)
            content := (
                {FALSE} -> spsyn(i+1)(FALSE),
                {TRUE} -> spsyn(i+1)(FALSE) + spsyn(i+1)(FALSE)
            )(spsyn(i+2)(FALSE) == "")
        } -> ({FALSE} -> Ident(ident), {TRUE} -> Funcy(content)) inherits CodePair
    )


    Reference := {TRUE} -> &String

    // Easy reference creation method
    template EasyRef := syn -= CodeSet(Funcy) -> (
        {FALSE} -> syn,
        {TRUE} ~ {
            refName := AsName(syn)
            name := SplitLast("$")(syn)
        } -> (
            {FALSE} -> DeclareIfAbsent(refName)(({TRUE} -> name) inherits Reference),
            {TRUE} -> DeclaredFuncy(refName)
        ) inherits CodePair
    )(Match("$\p{word}")(syn))


    // Import statement
    template ImportDecl := syn -= CodePairSet({FALSE} -> Ident, {TRUE} -> func) ~ {
        ident := syn(FALSE)
        flag := Include(" import ")(ident)
    } -> (
        
    )

    // Export statement
    template Export := (
        
    )
} -> (
    $TokenDeclaration -> TokenDecl

    $Reference -> Reference
    $EasyReference -> EasyReference
)