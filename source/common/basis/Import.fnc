inc ~ {
    Reference := TRUE -> % String

    // Easy reference creation method
    template EasyReference := ( &match -> "$\p{name}", &decl -> "(TRUE -> \"\1\") inherits Reference" )

    //
} -> (
    $Reference -> Reference
)