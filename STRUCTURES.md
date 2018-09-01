# Preface

Funcy is an unusual functional programming language, which doesn't have anything other than functions and forces you to use singular parameters and return to the functions.
Also, functions are generally not allowed to have any variables involved.
One of its specialty is that it does not allow multiline expression containing semicolon. The result should be singular, composite function calls.
Still, it's capable of all things programming languages can do, because it includes lambda calculus as its subset.
(It's known that lambda calculus is equivalent with universal turing machine)

# Definitions

## Mathematical

1. A ***funcy*** is a set of a pair (I, O), where all such I are distinct to each other and all such O are different.
  Here, I is called an *inset*, and O is called *outset*.

    1. Here union of insets I is called *domain*, and union of outsets O is called *image*.

    2. A *funcy* is called a *simple funcy*, when any outset of it is a singleton i.e. a set with exactly one element.
      It defines an unique function, and is equivalent with that one. The other funcies are called *virtual funcy*.
      Only simple funcies are recommended to be used as a parameter.

2. A ***type*** T of funcy F is a prototype funcy where for any pair (I, O) in F, there is a unique (J, P) in T
  where J contains I and P contains O. This implies funcy F can be used in place of funcy T - this meaning will be explained later.

    * It is said that F *inherits* T in this case.

    * A type needs to be explicitly stated if a function has a given type, as such check is not feasible.

3. ***Compound funcy*** is a special funcy which is defined as a union of several funcies. All of them should have disjoint insets.

4. ***Containment funcy*** is a special funcy which is simply {(I, O))} where I is a singleton.

5. ***Constant funcy** is a special funcy which is simply {(I, O)} where O is a singleton. It's a *simple funcy* as well.

5. ***Substitution*** F(V) where F is a declared funcy and V is a funcy is defined to be outset O if and only if
  there is a pair (I, O) in F where inset I contains V and O is a singleton.

## Specification

1. ***Declared funcy*** is a funcy with its name given - those funcies are said to be *declared*.

1. ***Native funcy*** is a special funcy which is defined internally.
  This includes Bool, Int, Float, Reference(e.g. $name), and such constants.
  Also, basic arithmetic operators and control statements are included.

2. ***Lambda funcy*** is a typical funcy where the result is given by expression based on the parameters.
  The expression is, basically, a declaration of another funcy when parameters are declared.

3. ***Compound funcy*** usually serves the purpose of easier code and simulation of muliple parameters.

4. ***Containment funcy*** usually serves the purpose to simulate member funcies and better type system.

# Syntax

1. ***Lambda funcy*** can have either of the corresponding forms:

  1. `F := P param -> (Function description here)`

  2. `F := P param ~ { Parameter-dependent function definitions here } -> (Function description here)`

2. A type of certain function F means set of functions which has maapping from F's domain into F's codomain.
For any function G which is typed as F, domain of G contains domain of F and codomain of G is contsined in codomain of F.
This allows G to be called with specifications of F without determination problem.

3. Using F as a type like `F G`, it's explicitly stated that G is typed as F. In this case, it is said that G inherits F.

4. There is a virtual function to help defining generalized statements. It declares the parameter type and result type.
It can be simply defined with parameter and return types, as in `F = (P | R)` where P and R are the parameter and return types(functions).

5. Compound is a special function which usually serve the purpose of easier code and benefits of compile-time validity check.
It's a mapping from Field to typed values, where field is usually a name to query the value.
It looks like this: `(name1 -> value1 , name2 -> value2)`
There's also virtual compound, where it contains undefined values. It looks like this: `(name1 -> Type1, name2 -> Type2)`.
This locks the mapping from name1 and name2 to inherit Type1, Type2. (explicitly)

* I have generics now, but looking forward to remove it - anyone know how?
