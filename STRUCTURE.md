# Preface

Funcy is a dependently-typed pure functional programming language.

(Outdated starting from here)
It's capable of all things programming languages can do, because it includes lambda calculus as its subset.
(It's known that lambda calculus is equivalent with universal turing machine)

(At some point would make a language based on sets/sequences)

Calculation of
`f g h`
1. evaluate f 
2. evaluate (f) g
3. evaluate (f g) h

# Language Frame (Basis Module)

## Mathematics

1. A ***Relation*** is a collection of input I and output O, which gives certain directed matching for the two. - This is superset of functions, and it's safe to assume that a relation is a function with multiple possible returns.

2. A ***Function*** is a relation where for every input, there is only one output.

2. A ***Pair*** I:O is a function with single input and single output.

3. A ***Tuple*** of a set V is a set representation of a sequence, which is recursively defined in this way:

    * An empty set is a recursive sequence.

    * For a recursive sequence S, (E, S) is a recursive sequence when E is an element of V.

4. A ***Relation*** is a set of pairs.

5. These sets are defined:

    * *Empty Set* is a set which doesn't contain any elements.

    * *Whole Set* is a set which contains every elements.

    * *Named Set* is a set of named sets, *Referenced Set* is a set of sets which can be referenced. Reference will be defined later. Note, that these are dynamic sets.

    * *Char* is a set of all kinds of possible glyphs in unicode.

## Word

0. Generally, ***Whitespace*** and ***Newline*** characters are used to segregate expressions.

1. ***Identifier*** is a word to represent certain identifier for a set.

    * All characters comprising an identifier needs to be a normal character.

4. ***Character Literal*** is a word to represent certain set for a character.

    * It's in the form of `'c'` where the character replaces `c`.

    * `'\"'` can be used for a quotation mark. Also special characters using backslash is accepted.

5. ***String Literal*** is a word to represent certain set of a string, which is a *recursive sequence* of characters.

    * This should begin and end with `"`. Also it shouldn't involve a newline.
    
    * `\"` can be used to put a quotation mark. Also special characters using backslash is accepted.

6. ***Brackets*** are words to represent certain groups/sequences.

    1. *Parenthesis*, `(` for opening and `)` for closing

    2. *Braces*, `{` for opening and `}` for closing

    3. *Brackets*, `[` for opening and `]` for closing

7. ***Comma*** is a word to represent separation. It's simply `,`

8. ***Equation*** are words to represent equality, which comprises the predicates.

    1. *Equality*, which is `==`. Comprises a predicate for an equality

    2. *Inequality*, which is `!=`. Comprises a predicate for an inequality.

9. ***Mapper*** is a word to represent a pair. It's simply `:`.

9. ***Char*** is a word for the *Char* set. It's simply `Char`.

## Clause

1. ***Set*** is a clause which evaluates a set.

2. ***Predicate*** is a clause which specifies a condition where an argument needs to suffice.

3. ***Declaration*** is a clause which declares a set with certain identifier to be an element of another set.

## Syntax

1. *Set*

    1. Referenced by *Identifier*: a set can simply referenced by its identifier, with `(identifier)`.

    2. Defined by *Enumeration*: `{set_1, set_2, ..., set_n}` where `set_i` is the set clause for i-th element. (`{}` becomes the empty set)

    3. Defined by *Predicate*

        * `{declaration(arg) || predicate(arg)}` where `declaration(arg)` is the declaration of the argument, and `predicate(arg)` is the predicate on the argument.

    4. Defined by *Pair*

        * `(set_input) : (set_output)` where `set_input` is the input set of the pair, and `set_output` is the output set of the pair.

    5. Defined by *Word*

        * Character Literal, String Literal and Char represents certain set.

2. *Predicate*

    * `(set_1)(equation)(set_2)` where set_1 and set_2 is the set clause to check equality for.

3. *Declaration*

    * `(identifier) -= (set)` where `identifier` is the identifier for the element, and the `set` is the set clause containing the element.

    * `(identifier)` in short, if the set is the whole set.

## Code Unit

1. ***Code Unit*** is the unit of a code which is represented as a file. 

2. A single code unit has a header and a single set in it.

3. First non-comment line of the header should be `"UnitName" in "packagename"`.

4. Other lines are for format inclusions. Use `include "fullUnitName"` to include certain format.

## Behavior

1. *Scope* is a range of a code where certain identifier reference could be used. E.g. Anything declared in set clause can't be used outside.

2. Any reference by identifier should be declared before its usage in the scope, and should be defined later.


# Format Module

***Format Module*** is a standard module for formatting, which is included in the language by default.


## Definition Submodule

### Syntax

1. *Declaration*

    1. *Definition*

        * `(identifier) := (set)` is equivalent with `(identifier) -= {(set)}`, which fixes the set with the identifier.


## Reference Submodule

A submodule which introduces references.

### Mathematics

1. Reference is represented as a pair ({}, ID) when ID is the String which represents certain identifier.

### Word

1. ***Reference Literal*** is a word to represent specific reference for certain identifier.

    * Starts with `$`, with preceding normal characters.

2. ***Expression*** `~` is a word to redirect to certain expression.

### Syntax

1. *Set*

    1. Defined as *Reference*

        * Reference Literal represents the reference.

    2. Defined by *Expression*

        * `(set_rel) ~ (set_expr)` where `set_rel` is the relation R, and `set_expr` is the expression with some unknown identifiers. the relation provides declarations for the unknown identifiers. Gives error if there are more than one result or there is no result.

2. *Predicate*



## Block Submodule

A submodule which introduces block for convenience of definition.

### Syntax

1. *Set*

    1. Defined by *Block*

        * `{(declaration_1); (declaration_2); ...; (declaration_n);}` where `declaration_i` is a declaration clause for i-th element.

2. *Declaration*


## Function Submodule

A submodule which introduces concept of functions, and convenience syntax for function definitions.

### Mathematics

1. A ***Function*** is a relation where for every input I there is at most one output O where (I, O) is in it.

2. A ***Substitution*** is a process of getting an output for certain input from a function, that is, finding output o from input i where pair (i, o) is in the function F. If it exists, the output is unique by definition of function. The output is represented as f(i).

3. ***Domain-Codomain*** is an operation which for a pair (I, O) gives a set of functions, where each element F is a function where set of all inputs comprises I, and all outputs are in O.

4. (Isn't this obsolete?)A relation F ***applies*** another relation G when for every input i of G, any pair (i, o) in F is also in G. This relation is transitive.

    * *Application* of a relation F is a set of all relations which applies F.

### Syntax

1. *Set*

    1. ***Lambda*** is used to define a function.

        * `(declaration(arg)) -> (set(arg))` where `declaration(arg)` is the declaration of the argument `arg`, and `set(arg)` is the set clause for the function evaluation.

    2. ***Domain-Codomain*** is used to define sets of functions using domain-codomain.

        * `(set_arg) ~> (set_ret)` where `set_arg` is the domain and `set_ret` is the codomain.

        * `(declaration(arg)) ~> (set_ret(arg))`

    3. ***Substitution*** is used to evaluate substitution.

        * `(set_rel)((set_arg))` where `set_rel` is the set clause representing the relation and `set_arg` is the set clause used as an argument.

    4. ***Image Evaluation*** is used to evaluate the image for certain subset of the domain.

        * `(set_rel)##(set_arg)` where `set_rel` is the set clause and ``

### Behavior

1. *Substitution* only works for relations 


# Arithmetics Module

***Arithmetics Module*** is a standard module for various arithmetics, which is included in the language by default.


## Set Arithmetics Submodule

A submodule which introduces set arithmetics.

### Mathematics

1. Union, which gives union of elements of a set.

    * The set `Union` is defined as a set of pairs (C, U) where U is the union of elements of C.

2. Intersection, which gives intersection of elements of a set.

    * The set `Intersection` is defined as a set of pairs (C, I) where I is the intersection of elements of C.

3. Complement, which gives complement of certain set.

    * The set `Complement` is defined as a set of pairs (S, C) where C is the complement of S.


## Boolean Submodule

A submodule which introduces boolean arithmetics.

### Mathematics

1. These are defined as an alias - these are called as boolean sets.

    1. ***FALSE*** for an empty set

    2. ***TRUE*** for a whole set

2. ***Bool*** is defined as a set of two elements, which are *TRUE* and *FALSE*

3. These *operators* are defined:

    1. ***And*** is an intersection of two boolean sets.

    2. ***Or*** is an union of two boolean sets.

    3. ***Not*** is a complement of a boolean set.

### Word

1. ***Bool Literals*** are either `TRUE` or `FALSE`. Self-explanatory.

2. *Operators*

    1. *And*, `/\` for and operator

    2. *Or*, `\/` for or operator

    3. *Not*, `-` for not operator

### Syntax

1. *Boolean Set* (Comprises *Set* clause)

    1. Evaluated by *Predicate*

        * `(predicate)` where `predicate` is the predicate clause (which is independent). This gives `(x | predicate)`.

    2. Evaluated by *Operation*

        * `(set_1)(operator)(set_2)` where `set_1` and `set_2` are set clause representing a boolean set.


## Integer Submodule

A submodule which introduces integer arithmetics and literals.

### Mathematics

1. ***Integer*** is a set of all integers.

2. These sets for integers represented by IEEE standard are defined:

    1. ***Byte*** is a set of signed integers in 8bit.

    2. ***UByte*** is a set of unsigned integers in 8bit.

    3. ***Short*** is a set of signed integers in 16bit.

    4. ***UShort*** is a set of unsigned integers in 16bit.

    5. ***Int*** is a set of signed integers in 32bit.

    6. ***UInt*** is a set of unsigned integers in 32bit.

    7. ***Long*** is a set of signed integers in 64bit.

    8. ***ULong*** is a set of unsigned integers in 64bit.

3. These operators are defined:

    1. ***Addition***

    2. ***Subtraction***

    3. ***Multiplication***

    4. ***Division*** or *quotient*

    5. 

### Words

1. ***Integer Literal*** is a word to represent certain set of an integer.

    * Decimal form: All characters comprising this literal needs to be numbers between 0 to 9. It needs to begin with nonzero value.

2. ***Operators***

### Syntax


## Float Submodule



### Mathematics

### Words

1. ***Floating Point Literal*** is a word to represent certain set of a floating point number.

    * Normal form: all number comprising this literal needs to have exactly one 


## Type Submodule

A submodule for type system.


## Conversion Submodule

Type conversion don't need the decided elements.
