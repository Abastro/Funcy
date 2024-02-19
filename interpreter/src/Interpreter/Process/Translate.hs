{-# LANGUAGE DataKinds #-}

module Interpreter.Process.Translate where

import Abstraction.Class.Categories
import Abstraction.Concrete.FreeClosed
import Abstraction.Types.Tuple
import CustomPrelude
import Data.Text qualified as T
import Interpreter.Process.FunComplete

-- import qualified Interpreter.Structure.Expr as Expr
-- import qualified Data.Vector as V

data InExpr
  = Var T.Text
  | Lambda T.Text InExpr
  | PairOf InExpr InExpr
  | BiCase InExpr InExpr
  | Operate Operation InExpr

data Operation
  = OpEval
  | OpFst
  | OpSnd
  | OpTagLeft
  | OpTagRight

-- | Lambda term of the category.
lambdaTerm :: T.Text -> InExpr -> FreeClosed Unit b
lambdaTerm x phi = Unsafe . liftMorph (completedLift x $ exprToFree phi)

-- | Translate an expression into a morphism from unit.
-- Since we are translating untyped terms, some coercions are necessary.
exprToFree :: InExpr -> FreeClosed Unit b
exprToFree = \case
  Var x -> Indet x
  Lambda x phi -> Unsafe . liftMorph (completedLift x $ exprToFree phi)
  PairOf l r -> Unsafe . together (exprToFree l) (exprToFree r)
  BiCase onL onR -> Unsafe . liftMorph (select (exprToFree onL) (exprToFree onR))
  Operate op e -> opOf (exprToFree e) op
 where
  opOf :: (forall c. FreeClosed a c) -> Operation -> FreeClosed a b
  opOf f = \case
    OpEval -> evaluate . f
    OpFst -> pickFst . f
    OpSnd -> pickSnd . f
    OpTagLeft -> Unsafe . tagLeft . f
    OpTagRight -> Unsafe . tagRight . f
  evaluate = Uncurried id
