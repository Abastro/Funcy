module Std.Data.Text

import Std.Type
import Std.Relation

module Impl
  #[Wrapper]
  construct Text : Type where
    Make : [basis| Data.Text |] -> Text
    derive (Eq, Ord)

Text : Type = Impl.Text

instance Semigroup Text where
  Impl.Text.Make left ++ Impl.Text.Make right = Impl.Text.Make ([basis| Data.Text.append |] (left, right))

instance Monoid Text where
  empty = ""
