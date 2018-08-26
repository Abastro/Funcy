Preface:

Funcy is an unusual functional programming language, which forces you to use singular parameters and return to the functions.
Also, functions are generally not allowed to have any variables involved.
One of its specialty is that it does not allow multiline expression containing semicolon. The result should be singular, composite function calls.
Still, it's capable of many things! (Not yet proved, but I think it's a Turing-Complete language)

Specifications:

1. A function is a mathematical function, which has one parameter and its domain and codomain - usually a type defines both.
It can be defined in this way: `F=(T parameter | {function description here})`. T will be discussed later.

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
