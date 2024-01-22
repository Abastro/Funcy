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

// Composition-chain state management
foo : { x: Int, y: Int } -> Int = {(->)|
  in {x, y}
  y := x - y
  y %= \case
    0 -> x
    1 -> x + 1
    n -> n
  x := x + y
  out (x * y)
}
