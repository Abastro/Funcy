/* 
 * Funcy is a language around idea of mathematical functions
 * Also, Compound is important in funcy - it's an easy way to introduce normal code for one-parameter function world
 * Also there is an interface function and simple function - interface function usually serves its role as a type
 * Here, 'Type value' means : the function 'value' inherits the function 'Type'. Surprisingly, it works well with traditional view on type
 * Inheritance is like this: The child function needs to be defined for all domain of the parent function, mapping all value to the codomain of the parent function.
 * Take a look!
 * (It has LGPL v3.0 license)
 */

Nullable = NULL // Compiler might only allow explicit NULL on Nullable values
Identity<F> = (F f | f)

FromInt<F> = (Integer | F)
ToInt<F> = (F f | 0) // Prototype - default implementation exists for now

FromBool<F> = (Boolean | F)
ToBool<F> = (F f | False)

Nullable Iterator<F> = (F value) // Inherited Nullable - Nullable values are declared. Here you can see the compound with a single value
HasNext<F, Iterator<F> I> = (I info | FALSE)
Next<F, Iterator<F> I> = (I | F) // Interface - lacks the name
Iterable<F, Iterator<F> I> = (HasNext<F,I> hasNext, Next<F,I> next) // Interface Function declaration as a interface compound - (Type1 name1, Type2 name2)

// Parameter is automatically wrapped into unnamed compound
Loop<F, Iterator<F>, Iterable<F, I>> = (Iterable<F, I> iterable, Function run |
    For<iterable(hasNext), iterable(next), run>)