/* 
 * Funcy is a language around idea of mathematical functions
 * Also, Compound is important in funcy - it's an easy way to introduce normal code for one-parameter function world
 * Also there is an interface function and simple function - interface function usually serves its role as a type
 * Here, 'Type value' means : the function 'value' inherits the function 'Type'. Surprisingly, it works well with traditional view on type
 * Inheritance is like this: The child function needs to be defined for all domain of the parent function, mapping all value to the codomain of the parent function.
 * Take a look!
 * (It has LGPL v3.0 license)
 */

// Implementation of Immutable Array on Funcy

import basics as None

// Namespace will be implicit. I just used this because I wanted to do all of this in one line
None::Iterator<F>             Iterator<F> = (Integer size, Integer index, Pointer<F> pointer, value -> OffsetGet<F>(ite(pointer, index)))
None::HasNext<F,Iterator<I>>  HasNext<F> = (Iterator<F> ite | ite(size) < ite(index))       // Inheritance forces the function to be applicable for parent cases
None::Next<F,Iterator<I>>     Next<F> = (Iterator<F> ite |
    IfElse(ite(size) < ite(index), ite,
        (Iterator<F> ite | <Iterator<Integer>>(ite(size), ite(index) + 1, Increase<F>(ite(pointer), 1), )), // Syntax sugar for a new compound function
        <Iterator<F>>NULL
    )
)
Setter<F>(Iterator<F> ite, Integer index, F value |
    <Iterator<F>> (size = ite(size), index = ite(index), pointer = OffsetSet<F>(ite(pointer), index, value))      
)

(FromInteger<F> fi, None::Iterable<F, Iterator<F>>) List<F> = (Iterator<F> head,
                                                        hasNext -> HasNext<F, Iterator<F>>, next -> Next<F, Iterator<F>> next,
                                                        indexer -> FromInteger<F>, setter -> Setter<F>, Integer index -> indexer(index)) // Complex compound declaration with inheritance

Set<F, List<F> L> = (L list, Integer index, F value |
    <List<F>>(list, head -> Setter<F>(list(head), index, value)) // Syntax sugar for compound creation
)

AsList<F> = (F... lists | <List<F>> (
    <List::Iterator<F>> (Size<F>(lists), -1, Pointerize<F>(lists))
) // Welp. For now I can't come up with better syntax for pointerizing this thing

