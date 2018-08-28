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
func Simple := func V -> (V -> V)
func Id := func V -> (V v -> v) inherits simple(V)      // Function definition. Generics are simply function calls
func FromInt := func V -> (V v -> 0) // Default implementation exists
native func Consumer := (func C, func I) -> ((C, I) -> C) // Maps to nothing here - an interface is first shown here. Native means it's fullfiled
native func toString := func -> String          // Another interface. This needs to be fleshed out

func Pair := (func T, func S) -> (T t, S s)     // Interface. Could be a type as well
func Compose := (func T, func S) -> ((Simple(T) trans1, Simple(S) trans2) ->
    ((T t, S s) -> trans1(t), trans2(s)))

// Pointer
native func Pointer := func F -> (F value)

native func NullPointer := func F -> Pointer(F)
native func NewPointer := func F -> (String id) -> Pointer(F)
native func NewPointer := func F -> (String id, Int size) -> Pointer     // Can assign another when the Parameter Type anywhere is different

native func OffsetGet := func F -> (Pointer(F) pointer, Int offset) -> Pointer    // Anonymous compound declaration to easily specify parameters (and result later)
native func OffsetSet := func F -> (Pointer(F) pointer, Int offset, V value) -> Pointer

// Iterators
func Ite := func F -> (F value)     // Virtual Function declared as a virtual compound

func HasNext := Ite(?) I -> (I ite -> FALSE) // No need to specify the parameter here - thus wildcard it
func Next := Ite(?) I -> (I -> I) // Virtual Function - damn, this reads bad
func Iterable := Ite(?) I -> (I head, HasNext hasNext, Next next) // Virtual Function declaration as a virtual compound - (Type1 name1, Type2 name2)

// Buffer
func StringState;       // Well, this is just a name
func Printer := Consumer(StringState, String)    // Un-genericized interface
func OutSite := (Printer print, StringState initial)
native func Console := (OutSite)() inherits OutSite     // Synthetic Sugar

func Print := OutSite site -> ((StringState state, func printed) -> site(print)(state, toString(printed)))

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

func AsArray = func F ->
    (String id, F a1, F a2, F a3) ~ {
        pointer := NewPointer<F>(id, 3);
    } -> (Array) (3, -1, pointer)

// Parameter is automatically wrapped into compound
func Loop = func F ->
    (Iterable(F) iterable, Consumer(F) run) ->
        For((iterable(head), ), iterable(hasNext), Compose(iterable(next),)) // Function version and Variable version is separately needed

func Main = String par -> Loop(AsArray("arr", 1, 2, 3), Print(Console))    // Compiler deal with guessing the type parameters. Also, lambdas
