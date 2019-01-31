{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Funcy.Interpreter.Values where

import Control.Applicative
import Data.List
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Funcy.Interpreter.Modules

type Name = String


-- Description for interpretation error handling
class (Monad d) => Description d where
    describeError :: String -> d a

newtype Desc a = Desc (Maybe a) deriving (Functor, Applicative, Monad)

instance Description Desc where
    describeError msg = Desc Nothing


data EValue ext = External ext | Composite Graph (Vector (EValue ext))


-- Translator to certain expression
newtype Translator expr = Translator { translateToExpr :: Name -> expr }

instance Functor Translator where
    fmap func trans = Translator (func . translateToExpr trans)

translateLocal :: DomainedFeature Translator expr => (Domain, Name) -> Maybe expr
translateLocal (domain, name) = do
    translator <- findFeature domain
    return $ translateToExpr translator name

translateAST :: DomainedFeature Translator expr => EValue (Domain, Name) -> Maybe (EValue expr)
translateAST = extractMaybe . fmap translateLocal


-- Or use Alternative?
extractMaybe :: EValue (Maybe ext) -> Maybe (EValue ext)
extractMaybe (External extern) = fmap External $ extern
extractMaybe (Composite egraph vals) = fmap (Composite egraph) (fmap extractMaybe $ vals)



-- EvalUnit definition
data EvalUnit sel tar = EvalUnit {
    -- Number of parameters
    numParam :: Int,

    -- Performs evaluation
    performEval :: sel -> [EValue tar] -> EValue tar
}


[ 0 ]
\a. [ floor a ]
\a. \b. [ (+) a b ]

[ ext [things] ]


evaluate :: ElementFeature (Modifier ext) ext => FuncPair ext -> FuncPair ext
evaluate (Extract pair) = case evaluate pair of
    Pair _ ext _ -> ext
    other -> Extract other
evaluate (Apply pair) = case evaluate pair of
    Pair True _ app -> app
    other -> Apply other
-- Now this is the interesting part
evaluate (Pair False dep (Extern fn)) = let applier = (deBroad $ featureOf fn) idCont in
    if numParam applier == 1 then performApply applier (dep : []) else Pair False dep (Extern fn)
evaluate other = other

-- Modules related with interpreters
newtype InterModule m = Intermodule m


