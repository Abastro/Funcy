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
// TODO Declaration of Set of which inherits the type
syntax Reference := TRUE -> String                 // Can't set it to String, ofc, as a Reference is different
Containment := func V -> (Field -> V)       // Syntax here allows the containment

// Templates here
native template(expr) $`v` -> Reference
template(func) `F` `v` := % ($v -> F) inherits Containment(F) // $ Declares the set which inherits all of it

template(func) [`func F` `V`] `expre` := F V -> expre
template(func) [`func F` `T`, `func G` `S`] `expre` := (F T, G S) -> expre
//template [F T, G S, H U] := expr -> ((F T, G S, H U) -> expr)

// Tutorials - Surely, a bad one, which means you won't get it if you're five
// Ah, right. You need to know programming basics for this.

// This is a common syntax to declare a 'funcy'. It describes, basically, every object in this language.
// Here, you can see a funcy is declared with the name on the left('TF') to have value on the right.
// `:=` is the declaration operator, which declares the funcy in the right with the name `TF` on the left.
// `Bool` is a funcy which serves as a type for booleans.
// `Bool switch` part declares switch as a boolean parameter, which can be used on the latter expression.
// Btw, you know what 'switch? 1 : 0'? No? Then halt reading and go back to programming basics.
TF := Bool switch -> switch? 1 : 0

// Substitutes the parameter with actual boolean value, which evaluates the funcy as well.
// Instructions unclear? Then just don't post issues on my github pls
ThisIsOne := TF(TRUE)

// `Int` is a funcy for integers - now first expression `Int i -> i+1` part *should* make sense.
// `thisIsZero` is the field name - it doesn't have the type declared, if you look at it carefully.
// So second expression `thisIsZero -> TF(FALSE)` maps field $thisIsZero to TF(FALSE) which is 0.
// The compound `(Expr1, Expr2)` means it can map both of the inputs of Expr1 and Expr2 to their outputs.
// This means they should both have disjoint input. (Usually it means differing type)
TF2 := (Int i -> i + 1, thisIsZero -> TF(FALSE))

// This is what I meant. Don't ask why these are named like that.
ThisIsTwo := TF2(1)
ThisIsZero := TF2($thisIsZero)

// The number of expression is not limited to two.
TF3 := (Int i -> i * 2, what -> 3,  "Hello" -> "World")
ThisIsThree := TF3($what)
ThisIsFour := TF3(2)
ThisIsWorld := TF3("Hello") // "World"
// ThisIsError := TF3("Welp") gives error because it does not have any mapping

// Now take a look at mundane declaration! Is this typedef?
TF3 := TF2

// From now on, virtual funcies would be discussed.
// Here, the parameter name is omitted. This imply it takes integer parameters, which gives boolean output.
// Of course, TF4(0) will give error as it does not have such mapping specified.
// Wait, you don't get why it's undefined? Screw that, kiddos. Stop reading this.
TF4 := Int -> Bool

// inherits declares the inheritance. Here it means TF5 can be used in the place of TF4 in substitution.
// It is applied to the last expression which is grouped.
TF5 := (Int i -> i != 0) inherits TF4

// You can look at the usage here. Note that every funcy could be a type.
// Anything which inherits funcy TF4 could have the type TF4.
// Here, val(0) could exist as val is not determined.
// Here you can see the purpose of virtual function - which exists to encapsulate things in opaque definitions.
TF6 := TF4 val -> val(0)

ThisISFalse := TF6(TF5)

// This means field $value will give integers.
// TF4($value) will give error, as it's not specified.
// Instead, it could be used to call inherited funcies with virtual type.
TF7 := Int value

TF8 := (value -> 7) inherits TF7

// Again, V($value) could exist as val is not determined.
TF9 := TF7 val -> val($value)

// Lazy evaluation should allow this - the applicability could be determined in compile-time, though.
ThisIsSeven := TF9(TF8)

// Now compounds being virtual: It's just compound of virtual funcies.
// No comments for this, figure it out yourself.
TF9 := (Int number, Int -> Int)
TF10 := (number -> 10, Int i -> i / 2) inherits TF10

TF11 := TF9 val -> val($number)
TF12 := TF9 val -> val(11)

ThisIsTen := TF11(TF10)
ThisIsFive := TF12(TF10)

// When it needs many declarations to evaluate the result, you can use parameter-dependent block in this way.
// Note that it thinks num is declared in the block, and gives new declarations from it
TFLong := Int num ~ {
    Int sqr := num * num
    Int result := num + 1
} -> result

// Basics are all covered now.
// Everything from now on is just shortcuts.
// TODO - describe shortcuts, especially with compounds - because they are annoying to write as whole.



// Lambda - (func V) -> (V v -> v) means template<typename V> anonymous(V v) { return v; } except it has no name
Self    := [V] V -> V
Id      := [V] (V v -> v) inherits Self(V)      // Function definition. Generics are simply function calls
FromInt := [V] Int -> V
ToBool  := [V] V -> Bool

native For := [F] (F initial, ToBool(F) condition, Self(F) increase) -> F

toString := [V] V -> String

Consumer := [V, I] (V value, I input) -> C // Maps to nothing here - an interface is first shown here. Native means it's fullfiled

Pair := [T, S] (T left, S right)     // Interface. Could be a type as well


// Pointer
native Pointer := [F] F value

native NullPointer := [F] Pointer(F)
native NewPointer := [F] (String id) -> Pointer(F)
native NewPointer := [F] (String id, Int size) -> Pointer     // Can assign another when the Parameter Type anywhere is different

native OffsetGet := [F] (Pointer(F) pointer, Int offset) -> Pointer    // Anonymous compound declaration to easily specify parameters (and result later)
native OffsetSet := [F] (Pointer(F) pointer, Int offset, V value) -> Pointer

// Iterators
Ite := [F] (F value)     // Virtual Funcy declared as a virtual compound

HasNext := [Ite(?) I] (I -> Bool) // No need to specify the parameter here - thus wildcard it (It's folded)
Next := [Ite(?) I] (I -> I) // Virtual Funcy
Iterable := [Ite(?) I] (I head, HasNext hasNext, Next next) // Virtual Funcy declaration as a virtual compound - (Type1 name1, Type2 name2)

ItePair := [F, V] Pair(Ite(F) ite, V val)
// Parameter is automatically wrapped into compound
Loop := [V, F] (Iterable(F) iterable, V initial, Consumer(V, F) consumer)
            -> For(
                    Pair(iterable($head), initial),
                    (Ite(F) ite, V val) -> iterable(hasNext)(ite),
                    (Ite(F) ite, V val) -> (iterable(next)(ite), consumer(val, ite(value)))
                ) // Auto-complete things here

// Buffer
StringState;                               // Well, this is just a name
Printer := Consumer(StringState, String)    // Un-genericized interface
OutSite := (Printer print, StringState initial)
native Console := (OutSite)() inherits OutSite     // Synthetic Sugar

Print := [OutSite site] (StringState state, func printed) -> site(print)(state, toString(printed)))
                inherits Consumer(StringState, func)

VarState = Field -> func;
Set := [V] (VarState state, Field field, ) -> 

namespace Array {
    Ite := [F] (Int size, Int index, Pointer pointer,
            value -> OffsetGet(F)(value(pointer), index))
        inherits None::Ite(F)     // Mixed interface compound declaration
    NullIte := [F] (0, 0, NullPointer(F)) inherits Ite(F)

    HasNext := [F] Ite(F) ite -> ite(size) < ite(index)
        inherits None::HasNext(Ite(F)))    // Inheritance forces the funcy to be applicable for parent cases

    Next := [F] ~ { // For declarations depending on the parameter
        I := Ite(F)
        internal := (I ite ~ {
            len := ite(size)
            ind := ite(index) + 1
            newp := OffsetGet(ite(pointer), 1)              // Auto-evaluated compound from funcy call
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

Main = String par -> Loop(AsArray("arr", 1, 2, 3), Console($initial), Print(Console)) != INVALID    // Compiler deal with guessing the type parameters. Also, lambdas
