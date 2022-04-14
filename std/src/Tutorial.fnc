module Tutorial {
// Same as { hide include }
import Prelude

// Hidden
hide myDef : Str = "Hello, World!"

action : IO Unit = act
  Sys.outln myDef
  _ <- myAct
  lift ()
  where
    myAct = act[IO]
      Sys.outln "Bye!"

}