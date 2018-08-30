# Preface

Funcy is an unusual functional programming language, which doesn't have anything other than functions and forces you to use singular parameters and return to the functions.
Also, functions are generally not allowed to have any variables involved.
One of its specialty is that it does not allow multiline expression containing semicolon. The result should be singular, composite function calls.
Still, it's capable of many things programming languages can do.

# Definitions

## Mathematical

1. A ***funcy*** from X to Y is a set of a pair (S, T), where S is a subset of X, T is of Y,
  all such S are distinct to each other and union of it is X.
  Here, S is called an *inset*, and T is called *outset*.

    1. Here X is called *domain*, and Y is called *codomain*. Union of outsets Y is caleled *image*.

    2. A *funcy* is called a *simple funcy*, when any outset of it is a singleton i.e. a set with exactly one element.
      It defines an unique function, and is equivalent with that one. The other funcies are called *virtual funcy*.

2. A ***type*** T of funcy F is a funcy which meets these conditions: identical in function X (?)

  1. 

  2. 

  3. A type needs to be explicitly stated if a function has a given type, as such check is not feasible.

3. Compound (function) is a special function which usually serve the purpose of easier code and simulation of muliple parameters.
It is defined by combining several functions with distinct domains, which is added to form one big domains and codomains.

4. Mapping (function) is a special function which usually serve the purpose to simulate sub-functions and better type system.
It is defined by mapping from a single Field to a single typed value, where the Field is referenced by its name.

    1. A normal mapping is a normal function which is a mapping.

    2. A virtual mapping is a virtual function which is defined by a domain of single Field, and the specified type.

## 


# Specifications

1. A non-special function can have either of the corresponding forms:

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
