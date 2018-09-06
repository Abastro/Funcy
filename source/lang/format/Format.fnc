import "lang/format/Format"
import -> export {
    // Basics
    syntax NativeSet := value -> % Bool

    // Templates here
    // Makes it possible to use more heuristic way to define conditional for typed funcy
    template(cond) ( &match -> "\p{decl} +\p{name}", &replace -> "\2 -= %\1" )
    // How to declare the template which involves the target?

    template(func)  ( &match -> "\p{name} *: *\p{func}", &replace -> "$\1:\2" )

    template(func) VirtContainer := ( &match -> "\p{decl} +\p{name}", &replace -> "\2 : %\1" )
    template(func) Generics := ( &match -> "[(\p{decl} +\p{name})*] \p{func}", &replace -> "\1 \2 -> \3")

    // Welp, how to formulate this in right way
    template(func) [`func F` `T`, `func G` `S`] `expr expre` := (F T, G S) -> expre
    //template [F T, G S, H U] := expr -> ((F T, G S, H U) -> expr)
}