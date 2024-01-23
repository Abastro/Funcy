# Structure

Funcy is a dependently-typed pure functional programming language.

## Formal grammar

### Tokens

| Token Name  | Description                 |
|-------------|-----------------------------|
| IDENT       | Identifier                  |
| QUALIFIED   | Qualified Identifier        |
| INTEGER     | An integer literal          |
| FLOAT       |                             |

### Grammar Rules

Module grammar

```ebnf
(* TODO Need to handle indentation rules *)

module = "module", QUALIFIED, imports, codespace;

imports =
  "include", QUALIFIED |
  "import", QUALIFIED, [ "as", IDENT ];
```

Codespace grammar

```ebnf
codespace = contexts, { declare, NEWLINE };

contexts = { var, NEWLINE };

var =
  "var", (typesig | "(", typesig, { ",", typesig } , ")");

declare = define | structure;

define =
  typesig, "=", expr |
  typesig, descriptions;

typesig = IDENT, [ ":", expr ];

signatures =
  contexts, { typesig, NEWLINE }, [ "with", codespace ];

structure =
  ("construct" | "destruct"), typesig, NEWLINE, signatures;

class =
  "class", _TODO, NEWLINE, signatures;

instance =
  "instance", _TODO, descriptions;

```

Descriptions

```ebnf

descriptions =
  "where", contexts, { describe };

describe =
  leftExpr, "=", expr | equate;

```
