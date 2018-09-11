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

0. Generally, ***Whitespace*** and ***Newline*** characters are used to segregate 

1. ***Identifier*** is a word to represent certain identifier for a set.

    * All characters comprising an identifier needs to be a normal character.

2. ***Integer Literal*** is a word to represent certain set of an integer.

    * Decimal form: All characters comprising this literal needs to be numbers between 0 to 9. It needs to begin with nonzero value.

3. ***Floating Point Literal*** is a word to represent certain set of a floating point number.

    * Normal form: all number comprising this literal needs to have exactly one 

4. ***Character Literal*** is a word to represent certain set for a character.

    * It's in the form of `'c'` where the character replaces `c`.

    * `'\"'` can be used for a quotation mark. Also special characters using backslash is accepted.

5. ***String Literal*** is a word to represent certain set of a string.

    * This should begin and end with `"`. Also it shouldn't involve a newline.
    
    * `\"` can be used to put a quotation mark. Also special characters using backslash is accepted.

6. ***Brackets*** are words to represent certain groups/sequences.

    1. *Parenthesis*, `(` for opening and `)` for closing

    2. *Braces*, `{` for opening and `}` for closing

    3. *Brackets*, `[` for opening and `]` for closing

7. ***Comma*** is a word to represent separation. It's simply `,`

8. ***Equation*** are words to represent equality, which comprises the predicates.

    1. *Equality*, which is `==`. Comprises predicate for an equality

    2. *Inequality*, which is `!=`. Comprises predicate for an inequality.

## Clause

1. ***Set*** is a clause which evaluates a set.

2. ***Declaration*** is a clause which declares a set with certain name to be an element of another set.

3. ***Definition*** is a clause which defines a set with certain name.

4. ***Block*** is a clause for a sequence of declarations and definitions.

5. ***Predicate*** is a clause which specifies a condition where an argument needs to suffice.

## Syntax

1. *Set*

    0. Referenced by *Identifier*: a set can simply referenced by its identifier, with `(identifier)`.

    1. Defined by *Enumeration*: `{set_1, set_2, ..., set_n}` where `set_i` is the set clause for i-th element. (`{}` becomes the empty set)

    2. Defined by *Predicate*

        * `declaration(arg) | predicate(arg)` where `declaration(arg)` is the declaration of the argument, and `predicate(arg)` is the predicate on the argument.

        * `declaration(arg) ~ { block(arg) } | predicate(arg)` where it's the same with the above with `block(arg)` being the block dependent on the argument.

2. *Declaration*

    * `(identifier) -= (set)` where `identifier` is the identifier for the element, and the `set` is the set clause containing the element.

3. *Definition*

    * `(identifier) := (set)` where `identifier` is the identifier for the set, and the `set` is the evaluation of the set.

4. *Block*

    * `(definition_1|declaration_1); (definition_2|declaration_2); ... (definition_n|declaration_n);` where `definition_i` or `declaration_i` is the defition/declaration clause for i-th element.

5. *Predicate*

    * `(set_1) (equation) (set_2)`

## Code Unit

0. ***Code Unit*** is the unit of a code which is represented as a file. 

1. A single code unit has a header and a single set in it.


## Interaction

1. *Scope*

2. Any reference by identifier should be declared before its usage in the scope.


# Function Module

## Mathematics

2. A ***Pair*** (i, o) is a set {i, {i, o}}. i is called as *input* and o is called as *output*.

3. A ***Relation*** is a set of pairs.

4. A ***Function*** is a relation, where the input of every pair element is distinct.

5. A ***Substitution*** is a process of getting an output for certain input from a function, that is, finding output o from input i where pair (i, o) is in the function F. If it exists, the output is unique by definition of function. The output is represented as f(i).

6. A relation F ***applies*** another relation G when for every input i of G, any pair (i, o) in F is also in G. This relation is transitive.

    * *Application* of a relation F is a set of all relations which applies F.

## Syntax

1. *Set*

    1. ***Lambda*** takes these syntax

    2. ***Compound*** requires `(set_1, set_2, ..., set_n)`

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
