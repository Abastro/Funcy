module Interpreter.Process.Parser where

import CustomPrelude
import Data.Char
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Void
import Interpreter.Structure.Expr qualified as Expr
import Interpreter.Structure.Pattern qualified as Pattern
import Interpreter.Structure.Structure
import Text.Megaparsec qualified as Parsec
import Text.Megaparsec.Char qualified as Parsec
import Text.Megaparsec.Char.Lexer qualified as Lexer

space :: Parsec.Parsec Void T.Text ()
space = Lexer.space Parsec.space1 Parsec.empty Parsec.empty

symbol :: String -> Parsec.Parsec Void T.Text T.Text
symbol = Lexer.symbol space . T.pack

lexeme :: Parsec.Parsec Void T.Text a -> Parsec.Parsec Void T.Text a
lexeme = Lexer.lexeme space

parens :: Parsec.Parsec Void T.Text a -> Parsec.Parsec Void T.Text a
parens = Parsec.between (symbol "(") (symbol ")")

stringLiteral :: Parsec.Parsec Void T.Text T.Text
stringLiteral = Parsec.label "string" $ T.pack <$> (Parsec.char '"' *> Parsec.manyTill Lexer.charLiteral (Parsec.char '"'))

tupleOf :: Parsec.Parsec Void T.Text a -> Parsec.Parsec Void T.Text (V.Vector a)
tupleOf parser = V.fromList <$> Parsec.label "tuple" (parens $ parser `Parsec.sepBy` symbol ",")

variable :: Parsec.Parsec Void T.Text T.Text
variable = lexeme (Parsec.takeWhile1P (Just "variable") isLetter)

-- >>> parse (T.pack "1 (2, 3) (4 5)")
-- Right ((1 (2, 3)) ((4 5)))

-- >>> import Interpreter.Bare qualified as Bare
-- >>> let Right expr = parse (T.pack "\\case { 1 -> \"hello\"; n -> \"world\" } 1")
-- >>> expr
-- >>> let (Right bare, decls) = Expr.runTranslate (Expr.translate mempty expr)
-- >>> bare
-- >>> decls
-- >>> Bare.runInterp decls (Bare.interpExpr mempty bare)
-- (\case{1 -> "hello"; n -> "world"} 1)
-- (decl:0 (1))
-- [Decl {normalArgNum = 0, cases = \case{1 -> ("hello"); () -> ("world")}}]
-- Right "hello"

-- TODO Use Pretty-printer

-- | Parse an expression.
parse :: T.Text -> Either String Expr.Expr
parse text = case Parsec.parse (parseExpr <* Parsec.hidden Parsec.eof) "expression" text of
  Left err -> Left $ Parsec.errorBundlePretty err
  Right expr -> Right expr

parseExpr :: Parsec.Parsec Void T.Text Expr.Expr
parseExpr = Parsec.label "expression" $ do
  Expr.applyToExpr <$> parseAtom <*> (V.fromList <$> Parsec.many parseExpr)

parseAtom :: Parsec.Parsec Void T.Text Expr.Expr
parseAtom = Parsec.label "atom" $ do
  Parsec.choice
    [ Expr.fromHead . Expr.Structural <$> Parsec.try (parseStr parseExpr),
      Expr.fromHead . Expr.Var <$> variable,
      Expr.fromHead . Expr.Case <$> parseCases,
      Parsec.label "parenthesized" $ parens undefined
    ]

parseCases :: Parsec.Parsec Void T.Text (Pattern.Cases T.Text Expr.Expr)
parseCases = Parsec.label "cases" $ do
  symbol "\\case"
  Parsec.between (symbol "{") (symbol "}") $ Pattern.Cases . V.fromList <$> parseCaseStmt `Parsec.sepEndBy` symbol ";"

parseCaseStmt :: Parsec.Parsec Void T.Text (Pattern.CaseStmt T.Text Expr.Expr)
parseCaseStmt = Pattern.CaseStmt <$> parsePattern <*> (symbol "->" *> parseExpr)

parsePattern :: Parsec.Parsec Void T.Text (Pattern.Pattern T.Text)
parsePattern = Parsec.label "pattern" $ do
  Parsec.choice
    [ Pattern.Var <$> variable,
      Pattern.Structural <$> parseStr parsePattern
    ]

parseStr :: Parsec.Parsec Void T.Text a -> Parsec.Parsec Void T.Text (Structure a)
parseStr subParser = Parsec.label "value" $ do
  Parsec.choice
    [ Integral <$> lexeme Lexer.decimal,
      Textual <$> lexeme stringLiteral,
      Tuple <$> tupleOf subParser
    ]
