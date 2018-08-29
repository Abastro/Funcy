/* 
 * Funcy is a language around idea of mathematical functions
 * Also, Compound is important in funcy - it's an easy way to introduce normal code for one-parameter function world
 * Also there is an interface function and simple function - interface function usually serves its role as a type
 * Here, 'Type value' means : the function 'value' inherits the function 'Type'. Surprisingly, it works well as traditional view on type
 * Inheritance is like this: The child function needs to be defined for all domain of the parent function, mapping all value to the codomain of the parent function.
 * Take a look!
 * (It has LGPL v3.0 license)
 */

// TODO Write specification for inheritance of virtual function
// Basic features
IntToString := Int -> String        // Virtual function of Int to String. Only used as a type, not a value
PairIS := (Int int, String string)  // Virtual compound function of Int and String. Only used as a type, not a value.
                                    // Can call PairIS(int) and PairIS(string) to get the int and string
ToEmpty := Int int -> "" inherits IntToString       // Function of Int to Empty String inheriting the virtual function.
Special := (int -> 0, string -> "") inherits PairIS // Compound function of 0 and "" inheriting the virtual compound function.

CompoundTrans := (PairIS pair, IntToString transform) -> transform(pair(string)) // Compound parameter, for easier and coherent passing

Test1 := Int useless -> ToEmpty(Special(int))   // Gives empty String
Test2 := CompoundTrans(Special, ToEmpty)        // Gives empty String

// Implementation of Immutable Array on Funcy
// Basics
// Lambda - (func V) -> (V v -> v) means template<typename V> anonymous(V v) { return v; } except it has no name
template [F V] := expr -> (F V -> expr)   // Template
template [F T, G S] := expr -> ((F T, G S) -> expr)
//template [F T, G S, H U] := expr -> ((F T, G S, H U) -> expr)

Self  := [V] V -> V
Id      := [V] (V v -> v) inherits Self(V)      // Function definition. Generics are simply function calls
FromInt := [V] Int -> V
ToBool := [V] V -> Bool

native For := [F] (F initial, ToBool(F) condition, Self(F) increase)

native toString := func -> String

Consumer := [V, I] (V value, I input) -> C // Maps to nothing here - an interface is first shown here. Native means it's fullfiled

Pair := [T, S] (T left, S right)     // Interface. Could be a type as well


// Pointer
native Pointer := [F] (F value)

native NullPointer := [F] Pointer(F)
native NewPointer := [F] (String id) -> Pointer(F)
native NewPointer := [F] (String id, Int size) -> Pointer     // Can assign another when the Parameter Type anywhere is different

native OffsetGet := [F] (Pointer(F) pointer, Int offset) -> Pointer    // Anonymous compound declaration to easily specify parameters (and result later)
native OffsetSet := [F] (Pointer(F) pointer, Int offset, V value) -> Pointer

// Iterators
Ite := [F] (F value)     // Virtual Function declared as a virtual compound

HasNext := [Ite(?) I] (I ite -> FALSE) // No need to specify the parameter here - thus wildcard it (It's folded)
Next := [Ite(?) I] (I -> I) // Virtual Function - damn, this reads bad
Iterable := [Ite(?) I] (I head, HasNext hasNext, Next next) // Virtual Function declaration as a virtual compound - (Type1 name1, Type2 name2)

ItePair := [F, V] Pair(Ite(F) ite, V val)
// Parameter is automatically wrapped into compound
Loop := [V, F] (Iterable(F) iterable, V initial, Consumer(C, F) consumer)
            -> For(
                    Pair(iterable(head), initial),
                    (Ite(F) ite, V val) -> iterable(hasNext)(ite),
                    (Ite(F) ite, V val) -> Pair(iterable(next)(ite), consumer(val, ite(value)))
                ) // Auto-complete things here

// Buffer
StringState;                               // Well, this is just a name
Printer := Consumer(StringState, String)    // Un-genericized interface
OutSite := (Printer print, StringState initial)
native Console := (OutSite)() inherits OutSite     // Synthetic Sugar

Print := [OutSite site] ([StringState state, func printed] site(print)(state, toString(printed)))
                inherits Consumer(StringState, func)

namespace Array {
    Ite := [F] (Int size, Int index, Pointer pointer,
            value -> OffsetGet(F)(value(pointer), index))
        inherits None::Ite(F)     // Mixed interface compound declaration
    NullIte := [F] (0, 0, NullPointer(F)) inherits Ite(F)

    HasNext := [F] Ite(F) ite -> ite(size) < ite(index)
        inherits None::HasNext(Ite(F)))    // Inheritance forces the function to be applicable for parent cases

    Next := [F] ~ { // For declarations depending on the parameter
        I := Ite(F)
        internal := (I ite ~ {
            len := ite(size)
            ind := ite(index) + 1
            newp := OffsetGet(ite(pointer), 1)              // Auto-evaluated compound from function call
        } -> (len < ind)? (I) (len, ind, newp) : NullIte(F)) inherits None::Next(I)
    } -> internal


    Setter := [F] (Ite(F) ite, Int index, F val) ~ {
        newp := OffsetSet(ite(pointer), index, value)
    } -> (Ite(F)) (ite, pointer -> newp) // Syntax sugar for compound declaration
}

Array := [F] (
    Array::Ite(F) head, // The only virtual element
    hasNext -> Array::HasNext(F), // Guess it could be implemented just here..
    next -> Array::Next(F),
    indexer -> FromInteger(F),
    setter -> Setter(F),
    indexer expose fi
) inherits Iterable(Array::Ite(F)) // Complex compound declaration with inheritance

Array::Set := [F] (Array(F) array, Integer index, F value) ~ {
    newHead := Setter(array(head), index, value);
} -> (Array(F)) (array, head -> newHead) // Syntax sugar for compound creation

AsArray = [F] (String id, F a1, F a2, F a3) ~ {
    pointer := NewPointer(F)(id, 3);
} -> (Array) (3, -1, pointer)

Main = String par -> Loop(AsArray("arr", 1, 2, 3), Print(Console))    // Compiler deal with guessing the type parameters. Also, lambdas
