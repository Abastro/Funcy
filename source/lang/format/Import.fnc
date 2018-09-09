// This is the capture structure composed of sets
Capture{
    "code @<string|0> anothercode @<parens|0> morecode",
    Args{"string", Indexed{0, Capture{"ThisIsString"}}},
    Args{"parens", Indexed{0, Capture{"inParenthesis"}}}
}

{
    SingletonSet := singleton | (x | singleton == {x}) != {}

    IndexedCharSet := indexed | (
        index -= IntSet | (
            char -= CharSet | indexed == {index, char}
        ) != {}
    ) != {}

    StringSet := string | (ic -= string | ic -= IndexedCharSet) == string

} ~ syntax {
    {
        0,
        stringPair | (
            prev -= CaptureSet ~ {
                prevString := element | (str -= (str -= StringSet | str -= prev) | element -= str) != {}
                indexSet := index -= IntSet | {index, '"'} -= prevString
                indexList := indexed | {indexSet, indexed} -= SortInt
                replaceInfo := repDesc | (
                    strIndex -= IntSet ~ {
                        beginIndex := @(IntSet) @u (index -= IntSet | {{2*strIndex}, {2*strIndex, index}} -= indexList)
                        endIndex := @(IntSet) @u (index -= IntSet | {{2*strIndex+1}, {2*strIndex+1, index}} -= indexList)
                        replaced := @(StringSet) @u (replacing -= StringSet | {{"string", strIndex}, replacing} -= ArgDescription)
                    } | repDesc == {beginIndex, endIndex, replaced} // Simple equality check, Easy to resolve
                ) != {}
                replaceArgs := @u (replacer | {replaceInfo, replacer} -= Replace)
                replaced := @(StringSet) @u (repStr | {index, repStr} -= replaceArgs)
                @u {prev, @c(StringSet)}
            } | stringPair == {{prev}, {prev, capture}} // Simple equality check, Easy to resolve
        ) != {}
    },
    {
        1,
        bracketPair ~ {

        }
    }

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