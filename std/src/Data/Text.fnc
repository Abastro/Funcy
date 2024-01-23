module Std.Data.Text

import Std.Type
import Std.Relation

module Impl
  #[Wrapper]
  construct Text : Type
    Make : [basis| Data.Text |] -> Text
    derive (Eq, Ord)

Text : Type = Impl.Text

instance Semigroup Text
  Impl.Text.Make left ++ Impl.Text.Make right = Impl.Text.Make ([basis| Data.Text.append |] (left, right))

instance Monoid Text
  empty = ""
