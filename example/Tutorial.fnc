module Tutorial

// Implied: import Prelude

// Hidden
hide myDef : Text = "Hello, World!"

action : () -[Eff]> () = {Eff|
  // in () can be omitted
  Sys.outLn myDef
  x := Sys.inLn
  Sys.outLn [format| {x} + {1} is {x+1} ]
  out ()
}

// TODO Profunctor vs. Category for the representation?

// Composition-chain state management
gcd : { x: Int, y: Int } -> Int = {(->)|
  in {x, y}

  // Implicit copying used
  case x < y
    True -> { (x, y) := (y, x) } // Reduces to x >= y case
    // Drops , selection used

  // (pre -[]> post) -> ((pre | con) -[]> (post | con))
  // (Bool, a) -[]> a

  if (y == 0) {
    out x
  } {
    r := x -[mod]- y
    out gcd {y, r}
  }
}


// foo : { x: Int, y: Int } -> Int = {(->)|
//   in {x, y}
//   y := x - y
//   y %= \case
//     0 -> x
//     1 -> x + 1
//     n -> n
//   x := x + y
//   out x * y
// }
