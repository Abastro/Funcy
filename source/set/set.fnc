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

(ToBool<F> crit, Iterable<F, I> ite) Set<F, Iterator<F> I> = (
    Iterable<F, I> iterable expose ite,
    ToBool<F> criterion expose crit) // Syntax sugar
// Well, Set is an interface after all