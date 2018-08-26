/* 
 * Funcy is a language around idea of mathematical functions
 * Also, Compound is important in funcy - it's an easy way to introduce normal code for one-parameter function world
 * Also there is an interface function and simple function - interface function usually serves its role as a type
 * Here, 'Type value' means : the function 'value' inherits the function 'Type'. Surprisingly, it works well with traditional view on type
 * Inheritance is like this: The child function needs to be defined for all domain of the parent function, mapping all value to the codomain of the parent function.
 * Take a look!
 * (It has LGPL v3.0 license)
 */

 // Implementation of Immutable Set on Funcy

import basics

None::Iterator<F>             Iterator<F> = (Integer size, Integer index, Pointer<F> pointer, value -> OffsetGet<F>(ite(pointer, index)))
None::HasNext<F,Iterator<I>>  HasNext<F> = (ListIterator<F> ite | ite(size) < ite(index))       // Inheritance forces the function to be applicable for parent cases
None::Next<F,Iterator<I>>     Next<F> = (ListIterator<F> ite |
    IfElse(ite(size) < ite(index), ite,
        (Iterator<F> ite | <ListIterator<Integer>>(ite(size), ite(index) + 1, Increase<F>(ite(pointer), 1), )), // Syntax sugar for a new compound function
        <Iterator<F>>NULL
    )
)
Setter<F>(Iterator<F> ite, Integer index, F value | <ListIterator<F>>   // Uses unnamed compound to easily specify parameters and results - generates new compound function, internally
    (size = ite(size), index = ite(index), pointer = OffsetSet<F>(ite(pointer), index, value))      
)                                                                               // OffsetSet needs to be internal

(ToBoolean<F> fi, Iterable<F, ListIterable<F>>) Set<F> = (List::Iterator<F> head,
                                                                hasNext -> List::HasNext<F, List::Iterator<F>>, next -> List::Next<F, List::Iterator<F>> next,
                                                                indexer -> FromInteger<F>, setter -> Setter<F>, t -> indexer(t)) // Complex compound declaration with inheritance

List::Set<List<F> L> = (L list, Integer index, F value |
    <List<F>>(list, head -> Setter<F>(list(head), index, value)) // Syntax sugar for compound creation
)

AsList<F> = (F... lists | <List<F>> (
    <List::Iterator<F>> (Size<F>(lists), -1, Pointerize<F>(lists))
) // Welp. For now I can't come up with better syntax for pointerizing this thing

Main = (P | Loop(AsList(1, 2, 3, 4), x -> Print[x]))    // Compiler deal with guessing the type parameters. Also, lambdas
// Main = (P | Loop[1, 2, 3, 4], x -> Print[x])         // Equivalent with above code