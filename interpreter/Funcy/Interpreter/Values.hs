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


-- Applier definition
data Applier ext = Applier {
    -- Number of parameters
    numParam :: Int

    -- Check for errors on application
    checkApply :: [Maybe ext] -> Desc (),

    -- Performs actual application
    performApply :: [Maybe ext] -> Maybe ext
}

-- Applier is expansive
instance Expansive Applier where
    expandWith cont applier = Applier {
        numParam = numParam applier
        checkApply = (checkApply applier) . (fmap $ flip (>>=) $ unwrap cont),
        performApply = (fmap $ wrap cont) . (performApply applier) . (fmap $ flip (>>=) $ unwrap cont)
    }


data ApplyFeature ext expr = ApplyFeature {
    hasApplier :: Name -> Bool
    applierOf :: Name -> Applier expr,

    hasModifier :: Name -> Bool
    modifierOf :: Name -> Containment ext expr -> Applier ext
}


instance Expansive (ApplyFeature ext) where
    expandWith cont appFeature = ApplyFeature {
        hasApplier = hasApplier appFeature
        applierOf = deProp $ expandWith cont $ Prop $ applierOf appFeature,
    
        hasModifier = hasModifier appFeature
        modifierOf = fmap deBroad . deProp $ expandWith cont $ Prop . fmap Broad $ modifierOf appFeature
    }


-- Func-Pair representation
data FuncPair ext = Pair Bool (FuncPair ext) (FuncPair ext) |
    Extract (FuncPair ext) | Apply (FuncPair ext) |
    Extern ext

instance Functor FuncPair where
    fmap function (Pair flag dep fn) = Pair flag (fmap function dep) (fmap function fn)
    fmap function (Extract pair) = Extract (fmap function pair)
    fmap function (Apply pair) = Apply (fmap function pair)
    fmap function (Extern ex) = Extern (function ex)

extractMaybe :: FuncPair (Maybe ext) -> Maybe (FuncPair ext)
extractMaybe (Pair flag dep fn) = liftA2 (Pair flag) (extractMaybe dep) (extractMaybe fn)
extractMaybe (Extract pair) = fmap Extract (extractMaybe pair)
extractMaybe (Apply pair) = fmap Apply (extractMaybe pair)
extractMaybe (Extern ex) = fmap Extern ex

upliftCont :: Containment b a -> Containment (FuncPair b) (FuncPair a)
upliftCont cont = Containment {
    wrap = fmap $ wrap cont,
    unwrap = extractMaybe . (fmap $ unwrap cont)
}

newtype ApplyFP ext tp = ApplyFP { getFeature :: ApplyFeature (FuncPair ext) (FuncPair tp) }

instance Expansive (ApplyFP ext) where
    expandWith cont afp = ApplyFP $ expandWith (upliftCont cont) (getFeature afp)

-- Modules related with interpreters
newtype InterModule m = Intermodule m


{-
evaluate :: FuncPair (Domain, Name) -> FuncPair ext
evaluate (Extract pair) = case evaluate pair of
    Pair _ ext _ -> ext
    other -> Extract other
evaluate (Apply pair) = case evaluate pair of
    Pair True _ app -> app
    other -> Apply other
evaluate (Extern (domain, name)) = (getApplyFP domain) name
evaluate (Pair False dep fn) = 
-}