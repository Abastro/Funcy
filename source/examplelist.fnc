/* 
 * Funcy is a language around idea of mathematical functions
 * Also, Compound is important in funcy - it's an easy way to introduce normal code for one-parameter function world
 * Also there is an interface function and simple function - interface function usually serves its role as a type
 * Here, 'Type value' means : the function 'value' inherits the function 'Type'. Surprisingly, it works well as traditional view on type
 * Inheritance is like this: The child function needs to be defined for all domain of the parent function, mapping all value to the codomain of the parent function.
 * Take a look!
 * (It has LGPL v3.0 license)
 */

// Implementation of Immutable Array on Funcy
// Basics
// Lambda - (V v -> v) means func(V v) { return v; } except it has no name
func Id := obj V -> (V v -> v)      // Function definition. Generics are simply function calls
func FromInt := obj V -> (V v -> 0) // Default implementation exists
native func Consumer := obj V -> var(V -> void) // Maps to nothing here - a prototype is first shown here. Native means it's fullfiled
native func toString := obj -> String

// Pointer
native func Pointer := func F -> (F value)

native var NullPointer := func F -> Pointer(F)
native var NewPointer := func F -> () -> Pointer(F)          // Variable accepts nothing as parameter as well
native var NewPointer := func F -> (Int size) -> Pointer     // Can assign another when the Parameter Type anywhere is different

native func OffsetGet := func F -> (Pointer(F) pointer, Int offset) -> Pointer    // Anonymous compound declaration to easily specify parameters (and result later)
native func OffsetSet := func F -> (Pointer(F) pointer, Int offset, V value) -> Pointer

// Iterators
func Ite := func F -> (F value)     // Virtual Function declared as a virtual compound

func HasNext := Ite(?) I -> (I ite -> FALSE) // No need to specify the parameter here - thus wildcard it
func Next := Ite(?) I -> (I -> I) // Virtual Function - damn, this reads bad
func Iterable := Ite(?) I -> (I head, HasNext hasNext, Next next) // Virtual Function declaration as a virtual compound - (Type1 name1, Type2 name2)

// Buffer
var IPrint := String -> Bool    // You know this is prototype by now
var OutSite := (IPrint print)   // Needs clarification, but enough for now
native var Console := (IPrint print) inherits OutSite

var Print := OutSite site -> var printed -> toString(printed)

func Next := func F ~ { // For declarations depending on the parameter
    I := Ite(F)
    internal := (I ite ~ {
        len := ite(size)
        ind := ite(index) + 1
        newp := OffsetGet(ite(pointer), 1)              // Auto-evaluated compound from function call
    } -> (len < ind)? (I) (len, ind, newp) : NullIte(F)) inherits None::Next(I)
} -> internal

namespace Array {
    func Ite := func F ->
        (Int size, Int index, Pointer pointer,
            value -> OffsetGet(F)(value(pointer), index))
        inherits None::Ite(F)     // Mixed interface compound declaration
    func NullIte := func F -> (0, 0, NullPointer(F)) inherits Ite(F)

    func HasNext := func F ->
        (Ite(F) ite -> (ite(size) < ite(index))
        inherits None::HasNext(Ite(F)))    // Inheritance forces the function to be applicable for parent cases

    func Next := func F ~ { // For declarations depending on the parameter
        I := Ite(F)
        internal := (I ite ~ {
            len := ite(size)
            ind := ite(index) + 1
            newp := OffsetGet(ite(pointer), 1)              // Auto-evaluated compound from function call
        } -> (len < ind)? (I) (len, ind, newp) : NullIte(F)) inherits None::Next(I)
    } -> internal


    func Setter := func F ->
        (Ite(F) ite, Int index, F val) ~ {
            newp := OffsetSet(ite(pointer), index, value)
        } -> (Ite(F)) (ite, pointer -> newp) // Syntax sugar for compound declaration
}

func Array := func F -> (
    Array::Ite(F) head,
    hasNext -> Array::HasNext(F),
    next -> Array::Next(F),
    indexer -> FromInteger(F),
    setter -> Setter(F),
    indexer expose fi
) inherits Iterable(Array::Ite(F)) // Complex compound declaration with inheritance

func Array::Set := func F ->
    (Array<F> Array, Integer index, F value) ~ {
        newHead := Setter(Array(head), index, value);
    } -> (Array<F>) (Array, head -> newHead) // Syntax sugar for compound creation

var AsArray = func F ->
    (F a1, F a2, F a3) ~ {
        pointer := NewPointer<F>(3);
    } -> (Array) (3, -1, pointer)

// Parameter is automatically wrapped into compound
func Loop = func F ->
    (Iterable(F) iterable, Consumer(F) run) ->
        For(iterable(head), iterable(hasNext), iterable(next), run) // Function version and Variable version is separately needed

var Main = String par -> Loop(AsArray(1, 2, 3), x -> Print(Console)(x))    // Compiler deal with guessing the type parameters. Also, lambdas
