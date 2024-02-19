{-# LANGUAGE DataKinds #-}

module Interpreter.Process.Translate where

import Abstraction.Class.Categories
import Abstraction.Concrete.FreeClosed
import Abstraction.Types.MappedTuple
import Abstraction.Types.Selector
import Abstraction.Types.Tuple
import CustomPrelude
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Interpreter.Process.FunComplete
import Interpreter.Structure.Expr qualified as Expr
import Interpreter.Structure.Pattern qualified as Pattern
import Interpreter.Structure.Structure qualified as Structure

data TranslateError

translateExpr :: Expr.Expr -> FreeClosed Unit b
translateExpr (Expr.Apply head args) = V.foldl apps (translateHead head) args
 where
  apps fn argE = evaluate . together (Unsafe . fn) (translateExpr argE)

translateHead :: Expr.Head -> FreeClosed Unit b
translateHead = \case
  Expr.Var x -> Indet x
  Expr.Lam x phi -> Unsafe . liftMorph (completedLift x $ translateExpr phi)
  Expr.Case (Pattern.Cases _) -> undefined
  Expr.Structural str -> translateStr str

translateStr :: Structure.Structure Expr.Expr -> FreeClosed Unit b
translateStr = \case
  Structure.Integral _ -> undefined -- TODO
  Structure.Textual _ -> undefined
  Structure.Tuple exprs -> withMapTuple (translateExpr <$> V.toList exprs) $ \tup ->
    Unsafe . Bundle tup
  Structure.Choice tag expr -> withAnySelector undefined (fromIntegral tag) $ \sel ->
    Unsafe . Tags sel . translateExpr expr

-- TODO Pattern matching - variable binding

-- | Translate cases; Assumes that tuple matching and tagged matching cannot be mixed.
translateCases :: [Pattern.CaseStmt T.Text Expr.Expr] -> FreeClosed a b
translateCases = \case
  -- We assume this case is not reachable
  [] -> fromVoid . Unsafe
  Pattern.CaseStmt patt expr : patts -> case patt of
    -- Irrefutable here
    Pattern.Var x -> completedLift x (translateExpr expr)
    Pattern.Structural (Structure.Tuple subPatts) -> undefined
    _ -> undefined
 where
  trCases patts = undefined
  handleChoices nTags patts = withMapTuple (map trCases grouped) $ \branches -> Choose branches . Unsafe
   where
    grouped = V.toList $ V.foldMap asGroup patts
    asGroup = \case
      Structure.Choice tag subPatt -> V.replicate nTags mempty V.// [(fromIntegral tag, [subPatt])]
      _ -> error ""

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
  Lambda x phi -> lambdaTerm x phi
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
