# Preface

Funcy is a dependently-typed pure functional programming language.


# Formal grammar

## Complex Tokens

| Token Name  | Description                 |
|-------------|-----------------------------|
| IDENT       | Identifier                  |
| QUALIFIED   | Qualified Identifier        |
| INTEGER     | An integer                  |
| FLOAT       | A floating point number     |


## Main Rules

```ebnf
(* TODO Need to handle indentation rules *)

file = { module };

module = "module", QUALIFIED, codespace;

codespace = "{" { define } "}";

bring =
  "include", QUALIFIED |
  "import", QUALIFIED;

namespace =
  IDENT, "=", ( bring | codespace );

declare = IDENT, [ ":", expr ];

define =
  bring | namespace | equate | structure;

describe =
  leftexpr, "=", expr | equate;

equate =
  declare, "=", expr |
  declare, "where", { describe };

strKey = "construct" | "destruct" | "class";
structure = strKey, declare,
  "where", { declare | decorate },
  [ "with", codespace ];

decorate = globalBind | derive;

(* NEED decl w/ space need be in parens *)
globalBind = "val", declare, { declare };
derive = "derive", { expr }

```
