# Preface

Funcy is an unusual functional programming language, which doesn't have anything other than *funcies* and forces you to use singular arguments and return to the functions. Here, the result should be singular, composite function calls.
It's capable of all things programming languages can do, because it includes lambda calculus as its subset.
(It's known that lambda calculus is equivalent with universal turing machine)
(Maybe it will have two objects, funcies and sets)

# Basis Module

## Definitions

### Mathematical

0. Let's assume the *set of everything* or the *whole set* exists. This is possible because only finite instructions could be added, thus the set of everything which can be expressed via code is also not too big to introduce contradiction.

1. A ***funcy*** is a set of a pair (I, O), where all such I are distinct to each other and all such O are different. Here, I is called an *inset*, and O is called *outset*.

    1. Here union of insets I is called *domain*, and union of outsets O is called *image*. Also, there is a unique pair (I, O) for any x in the domain. Here I is called *domain of x*, and O is called *image of x*. It could be easily proved that this is equivalent with a function which sends x to the image of x.

    2. A *funcy* is called a *simple funcy*, when any outset of it is a singleton i.e. a set with exactly one element. It defines an unique function, and is equivalent with that one. The other funcies are called *virtual funcy*.

2. ***Set funcy*** is a kind of simple funcy where the domain is the whole set whose outsets are either {TRUE} or {FALSE}. It represents the inset which corresponds to {TRUE}.

3. funcy F is said to ***inherit*** funcy T, where for any x in the domain of T, domain of x for F is a subset of domain of x for T, and image of x for F is also a subset of image of x for T. This means F can be used in place of T - this meaning will be explained later.

    * T could be used as a *type* of F in this case.

    * Inheritance needs to be explicitly stated, as such check is not feasible.

4. ***Compound funcy*** is a kind of funcy which is defined as a union of several funcies. Prior set takes advantage of deciding the image of x for any x in the union of domain.

5. ***Containment funcy*** is a kind of funcy which is simply {(I, O))} where I is a singleton.

6. ***Constant funcy*** is a kind of funcy which is simply {(I, O)} where O is a singleton. It's a *simple funcy* as well.

7. ***Substitution*** F(v), where F is a funcy and v is a funcy, is defined to be the outset O for a pair (I, O) in F where inset I contains v as an element.

### Specification

1. ***Declared funcy*** is a funcy which can be referenced. These funcies are said to be *declared*. It could either be fully defined or defined in a limited way.

2. ***Internal funcy*** is a kind of funcy which is declared internally. This includes `Bool`, `Int`, `Float`, `Character`, `String`. Also, basic arithmetic operators and control statements are included.

    * `Bool`, `Int`, `Float`, `Char` sends the total set to set of `Bool`/`Int`/`Float`/`Char` and its inheritors itself.

    * any boolean, integer, floating point constants sends the total set to the constant. They are constant funcies.

    * Basic arithmetic operators are self-explanatory - Some does take more than one arguments, which are modeled as compound containers. You can't reference them directly, anyway. (This could change, so that these operators could be a syntax sugar)

3. ***Lambda funcy*** is a kind of funcy where the result is given by expression based on the argument.
  The expression part is, basically, a declaration of another funcy when arguments are declared.

4. ***Compound funcy*** usually serves the purpose of easier code and simulation of muliple arguments.

5. ***Container funcy*** usually serves the purpose to simulate member funcies and better type system.

6. ***Native funcy*** is a special funcy which is declared to use native implementations or inter-operations.

## Syntax

### Basic Syntax

0. Every lines of funcy represents *declaration* of funcy, which links it with certain name so it could be referenced by the name.

1. ***Declared funcy*** requires the syntax `(name) := (funcy)` to declare the *funcy* with the name `name`. This forms *declaration*.

2. All of ***Internal funcy*** are internally declared.

3. ***Funcy Conditional*** is used to declare the funcy in a limited way. It takes `name -= (set)` to declare the funcy with the name which is within the set represented by the set funcy. The declared funcy in this way is quite restricted on usage.

    * Can be replaced with the `name` itself when the set is the total set.

4. ***Native funcy*** forms an *declaration* requiring `native (conditional(name))` where conditional represents *funcy conditional*, to declare funcy with the name `name` which is guaranteed to be in the set represented by the set funcy. This allows to lack implementation, so that it could get the implementation from basis / other language.

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
