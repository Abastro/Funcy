# Preface

Funcy is an unusual functional programming language, which doesn't have anything other than *funcies* and forces you to use singular arguments and return to the functions. Here, the result should be singular, composite function calls.
It's capable of all things programming languages can do, because it includes lambda calculus as its subset.
(It's known that lambda calculus is equivalent with universal turing machine)

# Definitions

## Mathematical

1. A ***funcy*** is a set of a pair (I, O), where all such I are distinct to each other and all such O are different. Here, I is called an *inset*, and O is called *outset*.

    1. Here union of insets I is called *domain*, and union of outsets O is called *image*. Also, there is a unique pair (I, O) for any x in the domain. Here I is called *domain of x*, and O is called *image of x*.

    2. A *funcy* is called a *simple funcy*, when any outset of it is a singleton i.e. a set with exactly one element. It defines an unique function, and is equivalent with that one. The other funcies are called *virtual funcy*.

2. A ***type*** T of funcy F is a prototype funcy, where for any x in the domain of T, domain of x for F is a subset of domain of x for T, and image of x for F is also a subset of image of x for T. This means F can be used in place of T - this meaning will be explained later.

    * It is said that F *inherits* T in this case.

    * A type needs to be explicitly stated if a function has a given type, as such check is not feasible.

3. ***Compound funcy*** is a special funcy which is defined as a union of several funcies. All of them should have disjoint insets.

4. ***Containment funcy*** is a special funcy which is simply {(I, O))} where I is a singleton.

5. ***Constant funcy*** is a special funcy which is simply {(I, O)} where O is a singleton. It's a *simple funcy* as well.

5. ***Substitution*** F(V), where F is a declared funcy and V is a simple funcy, is defined to be an element of outset O if and only if there is a pair (I, O) in F where inset I contains V and O is a singleton.

## Specification

1. ***Declared funcy*** is a funcy with its name given - those funcies are said to be *declared*.

2. ***Internal funcy*** is a special funcy which is declared internally. This includes `Bool`, `Int`, `Float`, `Reference`(parent of references e.g. `$name`), and such constants. Also, basic arithmetic operators and control statements are included. (Reference might be deferred later)

    * `Bool`, `Int`, `Float`, `Byte`, `String` sends the total set to set of `Bool`/`Int`/`Float`/`Byte`/`String` and its inheritors itself

    * any boolean, integer, floating point constants sends the total set to the constant. They are constant funcies.

    * Reference sends {`TRUE`} to set of String and its inheritors.

    * any reference sends {`TRUE`} to the name of the reference. It is a constant funcy.

    * Basic arithmetic operators are self-explanatory - Some does take more than one arguments, which are modeled as compound containments. You can't reference them directly, anyway. (This could change, so that these operators could be a syntax sugar)

3. ***Lambda funcy*** is a typical funcy where the result is given by expression based on the arguments.
  The expression part is, basically, a declaration of another funcy when arguments are declared.

4. ***Compound funcy*** usually serves the purpose of easier code and simulation of muliple arguments.

5. ***Containment funcy*** usually serves the purpose to simulate member funcies and better type system.

6. ***Native funcy*** is a special funcy which is declared to use native implementations.

# Syntax

## Basic Syntax

0. Every lines of funcy represents *declared funcy* - this is called ***declaration***, opposed to ***expression*** which represents an actual funcy and ***presentation*** which represents a set of funcy. *expression* can be regarded as singleton *presentation*

1 ***type*** requires an expression which represents declared funcy.

1. ***Declared funcy*** requires the syntax `name := (expression)` to declare the funcy given by expression with the name `name`. Its name forms an *expression*.

2. All of ***Internal funcy*** are internally declared, so that its name can be used to form *expression*

3. ***Lambda funcy*** forms an *expression*. This takes these syntax when `P` is a declared funcy:

    * `P param -> (presentation)` for simplistic ones - param is assumed to be declared in the scope. Sends param to the set given by the presentation.

    * `P param ~ {declaration(s)} -> (presentation)` for complex ones - param is assumed to be declared in the scope. Does the same as above.

    * `P -> (presentation)` for constant/virtual - sends set of funcies which inherits P to the presentation.

    * Note that `func` is used when there is no type to specify.

4. ***Compound funcy*** forms an *expression* requiring `(expression_1, expression_2, ..., expression_n)` where n can be any positive integer, which means union of these funcies given by each expression.

5. ***Containment funcy*** forms an *expression* requiring `name : (presentation)` where funcy `name` is declared.

6. ***Substitution*** forms an *expression* requiring `name(expression)` where the (simple) expression is applied to the declared funcy `name`. It gives a *declared funcy* when the expression argument is also declared.

7. ***Inheritance*** is used to declare inheritance. `expression inherits name` declares the expression to inherit the declared funcy of `name` - it's useful when the expression represents a *declared funcy*.

8. ***Inheritance Operator*** is used to create *presentation* from an expression, with syntax of `%(expression)`. It gives a set of funcies which inherits the funcy given by the expression.

9. ***Native funcy*** forms an *declaration* requiring `native name := (expression)` to declare funcy with the name `name`, where the expression represents *virtual funcy*. This gets the implementation from other language.

10. ***Wildcard funcy*** `?` forms an *expression* representing declared funcies which is used as types to reduce additional bloats. Can be applied when the parameter is not involved in the syntax being used.

## Shortcut Syntax

1. ***Template Statement*** is used to make a shortcuts for *declaration / expression / presentation*. It requires the syntax of `template(tmpltype) syntaxPre := syntaxPost` which converts the `syntaxPre` in `tmpltype` scope to `syntaxPost`. This applies additional conversion rule for less bloats.

    * `tmpltype` defines the scope to check for the syntax.

    * `syntaxPre` could involve `argument` to match names and such. It could be either name(`T`), funcy(`func F`), or expression(`expr E`). It is used in the `syntaxPost`

2. ***Referential Containment Compound(RCC) Inlining*** is used to inline the RCC funcy declaration/expression.

    * When used as an *argument*, declaration of RCC is inlined in an argument, and cull the argument name out from the lambda funcy. Because the name to reference is missing, expression of the argument `param($ref)` becomes `ref` instead. (This feature might be in additional module)

    * When used as an *input* of substitution, expression of RCC is automatically inherits the argument type if it wasn't. Also it could make use of the format described below.

    * Any *expression* of RCC can be simplified. E.g. `(rcc) inherits P` could be `(P) (rcc)`. Moreover, if it matches the format of P, it can be simplified further to remove the reference names. Order becomes important in this case.

3. ***Declared argument Prediction*** is used to cull the obvious arguments from a substitution out so the code could be less verbose. It predicts arguments in these cases:

    * When a lambda function uses the argument to specify the type in its expression (In this case, declared funcy is favored as an argument). It guesses from the type required, and latter expressions if it uses more substitution.