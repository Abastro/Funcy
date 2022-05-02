module Tutorial {
// Same as { hide include }
import Prelude

// Hidden
hide myDef : Str = "Hello, World!"

action : Unit `Eff` Unit = act
  Sys.outln myDef
  _ <- myAct
  end ()
  where
    myAct = act[Eff]
      Sys.outln "Bye!"

}