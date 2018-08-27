/* 
 * Funcy is a language around idea of mathematical functions
 * Also, Compound is important in funcy - it's an easy way to introduce normal code for one-parameter function world
 * Also there is an interface function and simple function - interface function usually serves its role as a type
 * Here, 'Type value' means : the function 'value' inherits the function 'Type'. Surprisingly, it works well as traditional view on type
 * Inheritance is like this: The child function needs to be defined for all domain of the parent function, mapping all value to the codomain of the parent function.
 * Take a look!
 * (It has LGPL v3.0 license)
 */

// Implementation of Immutable List on Funcy

Nullable = NULL // Compiler might only allow explicit NULL on Nullable values

// Basics
template <obj V>                    // This assumes generics
func Id = V v -> v
func FromInt = V v -> 0             // Prototype - default implementation exists

// Pointer
template <func F>
native func Pointer : Nullable = (V value) // Inherited Nullable - Nullable values are declared. Here you can see the compound with a single value

native var NewPointer = () -> Pointer                       // Variable accepts nothing as parameter as well - which only matches NULL, essentially
native var NewPointer = (Int size) -> Pointer               // Can assign another when the Parameter Type is different

native func OffsetGet = (Pointer pointer, Int offset) -> Pointer    // Annonymous compound declaration to easily specify parameters (and result later)
native func OffsetSet = (Pointer pointer, Int offset, V value) -> Pointer

// Iterators
template <func F>
func Ite : Nullable = (F value)     // Virtual Function declared as a virtual compound

template <func F, Ite<F> I>
HasNext = I ite -> FALSE
Next = I -> I // Virtual Function
Iterable = (I head, HasNext hasNext, Next next) // Virtual Function declaration as a virtual compound - (Type1 name1, Type2 name2)

// Buffer
var Out // This needs more specification - it is incomplete!

var Printer = obj output -> Bool
native func Print = Out write -> Printer printer
native var Console : Out

namespace List {
    template <func F, I := Ite<F>>
    func Ite : None::Ite = (Int size, Int index, Pointer pointer,
                        value -> OffsetGet(value(pointer), index))          // Mixed interface compound declaration

    func HasNext : None::HasNext = I ite -> (ite(size) < ite(index))        // Inheritance forces the function to be applicable for parent cases
    func Next    : None::Next    = I ite ~ {
        len := ite(size)
        ind := ite(index) + 1
        newp := OffsetGet(ite(pointer), 1)                                  // Auto-evaluated compound from function call
    } -> (len < ind)? <I> (len, ind, newp) : <I> NULL

    func Setter = (Ite<F> ite, Int index, F val) ~ {
        newp := OffsetSet(ite(pointer), index, value)
    } -> <I> (ite, pointer -> newp)
}

template <func F, I := List::Ite<F>>
func List = (
    List::Ite head,
    hasNext -> List::HasNext,
    next -> List::Next,
    indexer -> FromInteger<F>,
    setter -> Setter<F>,
    indexer expose fi
) // Complex compound declaration with inheritance

template <func F>
func List::Set = (L list, Integer index, F value) ~ {
    newHead := Setter(list(head), index, value);
} -> <List<F>> (list, head -> newHead) // Syntax sugar for compound creation

var AsList = (F a1, F a2, F a3) ~ {
    pointer := NewPointer<F>(3);
} -> <List> (3, -1, pointer)

// Parameter is automatically wrapped into compound
template <F, I := Ite<F>, C := Iterable<F>>
func Loop = (Iterable iterable, obj run) ->
    For(iterable(head), iterable(hasNext), iterable(next), run) // Function version and Variable version is separately needed

template <func P>
var Main = P par -> Loop(AsList(1, 2, 3), x -> Print(Console)(x))    // Compiler deal with guessing the type parameters. Also, lambdas
// Main = (P | Loop[1, 2, 3], x -> Print(x))        // Equivalent with above code

// TODO Only variable declaration is left
