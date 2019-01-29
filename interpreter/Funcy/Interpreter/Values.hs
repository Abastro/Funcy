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


-- EvalTree (Evaluated Value) [Entries where value is used]
data EvalTree ext = ExtValue ext | EvalTree (EvalTree ext) [Evaltree ext]


-- Func-Pair representation
data FuncPair ext = Pair Bool (FuncPair ext) (FuncPair ext) |
    Extract (FuncPair ext) | Apply (FuncPair ext) |
    Extern ext

instance Functor FuncPair where
    fmap function (Pair flag dep fn) = Pair flag (fmap function dep) (fmap function fn)
    fmap function (Extract pair) = Extract (fmap function pair)
    fmap function (Apply pair) = Apply (fmap function pair)
    fmap function (Extern ex) = Extern (function ex)


-- Translator to certain expression
newtype Translator expr = Translator { translateToExpr :: Name -> expr }

instance Functor Translator where
    fmap func trans = Translator (func . translateToExpr trans)

translateLocal :: DomainedFeature Translator expr => (Domain, Name) -> Maybe expr
translateLocal (domain, name) = do
    translator <- findFeature domain
    return $ translateToExpr translator name

translateAST :: DomainedFeature Translator expr => FuncPair (Domain, Name) -> Maybe (FuncPair expr)
translateAST = extractMaybe . fmap translateLocal


extractMaybe :: FuncPair (Maybe ext) -> Maybe (FuncPair ext)
extractMaybe (Pair flag dep fn) = liftA2 (Pair flag) (extractMaybe dep) (extractMaybe fn)
extractMaybe (Extract pair) = fmap Extract (extractMaybe pair)
extractMaybe (Apply pair) = fmap Apply (extractMaybe pair)
extractMaybe (Extern ex) = fmap Extern ex



-- Applier definition
data Applier expr = Applier {
    -- Number of parameters, 0 if invalid
    numParam :: Int,

    -- Check for errors on application
    --checkApply :: [Maybe ext] -> Desc (),

    -- Performs actual application
    performApply :: [FuncPair expr] -> FuncPair expr
}


-- Modifier is applier which acts on broader type
-- Represents Containment ext m -> Applier ext
type Modifier ext = Broad Applier ext

[ 0 ]
\a. [ floor a ]
\a. \b. [ (+) a b ]

[ ext [things] ]

-> FuncPair (Lambda ext)

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


