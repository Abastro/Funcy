import "common/basis/Import"
import -> export {
    import None := import "common/basis/Format", "common/basis/Types"

    ListIte := [T] (
        T value,

    ) inherits Ite(T)

    List := [T] (
        ListIte(T) ite,
        hasNext : [T] LiteIte(T) ite -> 
    ) inherits Iterable(ListIte(T))

}