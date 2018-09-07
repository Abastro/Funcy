# Preface

Funcy is an unusual programming language, which doesn't have anything other than *sets*.
It's capable of all things programming languages can do, because it includes lambda calculus as its subset.
(It's known that lambda calculus is equivalent with universal turing machine)

# Basis Module

## Mathematical Basis

0. Let's assume the *set of everything* or the *whole set* exists. This is possible because only finite instructions could be added, thus the set of everything which can be expressed via code is also not too big to introduce contradiction.

1. A ***Set*** is a group of objects. It has containment relationship, which is represented as .

2. A ***Tuple*** (i1, i2, ..., in) is a set {i1, {i1, i2}, {i1, i2, i3}, ..., {i1, i2, ..., in}}. It has a clearly defined order.

    * A *Pair* is a tuple with 2 elements. For a pair (i, o), i is called as *input* and o is called as *output*.

3. A ***Relation*** is a set of pairs.

4. A ***Function*** is a relation, where the input of every pair element is distinct.

5. A ***Substitution*** is a process to get an output for certain input from a function, that is, finding output o from input i where pair (i, o) is in the function F. If it exists, the output is unique by definition of function. The output is represented as f(i).

6. A relation F ***applies*** another relation G when for every input i of G, any pair (i, o) in F is also in G. This relation is transitive.

    * *Application* of a relation F is a set of all relations which applies F.

7. These sets are defined:

    * *Empty Set* is a set which doesn't contain any elements.

    * *Whole Set* is a set which contains all elements.

    * *Named Set* is a set of named sets, *Referenced Set* is a set of sets which can be referenced. Reference will be defined later. Note, that these are dynamic sets.

## Segments

0. ***Segment*** is part of a code which specifies/evaluates some set / condition / process.

1. ***Declaration*** is a segment which declares a set to be an element of another set. (`(name) -= (set)`)

2. ***Definition*** is a segment which links certain name with a set. (`(name) := (set)`)

3. ***Set builder notation*** is a segment which evaluates a set with parent set and certain condition(function to bool). (`{declaration(arg) | condition(arg)}`)

4. ***Set enumeration*** is a segment which evaluates a set of finite sets. (`{set_1, set_2, ..., set_n}`)

5. ***Substitution*** is a segment which (`(name) join (set)`)

## Syntax

### Basic Syntax

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
