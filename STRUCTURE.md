# Preface

Funcy is an unusual programming language, which doesn't have anything other than *sets*.
It's capable of all things programming languages can do, because it includes lambda calculus as its subset.
(It's known that lambda calculus is equivalent with universal turing machine)

# Basis Module

## Mathematics

0. Let's assume the *set of everything* or the *whole set* exists. This is possible because only finite instructions could be added, thus the set of everything which can be expressed via code is also not too big to introduce contradiction.

1. A ***Set*** is a group of objects. It has containment relationship as a predicate.

2. These sets are defined:

    * *Empty Set* is a set which doesn't contain any elements.

    * *Whole Set* is a set which contains every elements.

    * *Named Set* is a set of named sets, *Referenced Set* is a set of sets which can be referenced. Reference will be defined later. Note, that these are dynamic sets.

## Word

1. ***Identifier*** is a word which represents certain identifier for a set.

2. ***Integer Literal*** is a word which represents certain set of an integer.

3. ***Floating Point Literal*** is a word which represents certain set of a floating point number.

4. ***Character Literal*** is a word which represents certain set of a character.

5. ***String Literal*** is a word which represents certain set of a string.

6. ***Operator*** is a word to represent operators.

    1. Basic arithmetic operators.

        * `+` for addition
        
        * `-` for subtraction
        
        * `*` for multiplication
        
        * `/` for division
        
        * `%` for mod

## Clause

1. ***Set*** is a clause which evaluates a set.

2. ***Declaration*** is a clause which declares a set with certain name to be an element of another set.

4. ***Definition*** is a clause which defines a set with certain name.

3. ***Block*** is a clause for a sequence of declarations and definitions.

5. ***Predicate*** is a clause which specifies a condition where an argument needs to suffice.

## Syntax

1. *Declaration*

    * `(identifier) -= (set)` where `identifier` is the identifier for the element, and the `set` is the set clause containing the element. 

2. *Definition*

    * `(identifier) := (set)` where `identifier` is the identifier for the set, and the `set` is the evaluation of the set.

3. *Set*

    0. *Identifier*

    1. Defined by *Enumeration*: `{set_1, set_2, ..., set_n}` where `set_i` is the set clause for i-th element. (`{}` becomes the empty set)

    2. Defined by *Predicate*

        * `declaration(arg) | predicate(arg)` where `declaration(arg)` is the declaration of the argument, and `predicate(arg)` 

        * `declaration(arg) ~ { definitions(arg) } | predicate(arg)` where `definitions(arg)` is the multiple definitions separated by semicolon `;`.

## Code Unit

0. ***Code Unit*** is the unit of a code which is represented as a file. 

1. A single code unit has a header and a single set in it.

# Function Module

## Mathematics

2. A ***Pair*** (i, o) is a set {i, {i, o}}. i is called as *input* and o is called as *output*.

3. A ***Relation*** is a set of pairs.

4. A ***Function*** is a relation, where the input of every pair element is distinct.

5. A ***Substitution*** is a process of getting an output for certain input from a function, that is, finding output o from input i where pair (i, o) is in the function F. If it exists, the output is unique by definition of function. The output is represented as f(i).

## Syntax

6. A relation F ***applies*** another relation G when for every input i of G, any pair (i, o) in F is also in G. This relation is transitive.

    * *Application* of a relation F is a set of all relations which applies F.

5. ***Lambda funcy*** takes these syntax, when conditional represents *funcy conditional*, set represents *set funcy* and decl represents *declarations*:

    * `(conditional(arg)) -> (set(arg))` for simplistic ones - arg is assumed to be declared in the scope. Sends arg to the set described with the argument.

    * `(conditional(arg)) ~ {decls(arg)} -> (set(arg))` for complex ones - arg is assumed to be declared in the scope. Does the same as above.

    * `conditional(arg)` could be replaced by the argument restricting set itself when the result set is constant to the arguemtn.

5. ***Compound funcy*** requires `funcy_1, funcy_2, ..., funcy_n` where n can be any positive integer, which means union of these funcies given by each expression. Things on the left have higher priority.

6. ***Substitution*** gives a set funcy which represents the outset of the funcy. This requires `name-(funcy)` where the expression is applied to the declared funcy `name`. In case of `name` declared with funcy conditional, it can only be applied in the parent form which all elements of the set inherit. In case of `funcy` declared with funcy conditional, it can only be applied when  It gives a declared funcy when the expression argument is also declared.

7. ***Inheritance*** is used to declare inheritance. `(funcy) inherits (name)` declares the funcy to inherit the declared funcy of `name`. The funcy defined by this syntax itself should be declared, i.e. can be referenced as well.

8. ***Inheritance operator*** requires `%(name)`, which gives a set funcy which represents the set of funcy which inherits the declared funcy `name` excluding itself.

9. ***Singleton opearator*** requires `{name}`, which gives a set funcy which represents the singleton containing the funcy `name`.

### Shortcut Syntax

1. ***Template Statement*** is used to make a shortcuts for *declaration / expression / presentation*. It requires the syntax of `template(tmpltype) syntaxPre := syntaxPost` which converts the `syntaxPre` in `tmpltype` scope to `syntaxPost`. This applies additional conversion rule for less bloats.

    * `tmpltype` defines the scope to check for the syntax.

    * `syntaxPre` could involve `argument` to match names and such. It could be either name(`T`), funcy(`func F`), or expression(`expr E`). It is used in the `syntaxPost`
