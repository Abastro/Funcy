import "lang/format/Import"
import -> export {
    import None := import "lang/format/Format", "lang/generics/Generics", "lang/collection/Ites";

    ListIte := [T] (
        T value,

    ) inherits Ite(T);

    List := [T] (
        ListIte(T) ite,
        hasNext : [T] LiteIte(T) ite -> 
    ) inherits Iterable(ListIte(T));

}