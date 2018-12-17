-- Module System for Funcy (Most of this is just to simulate duck typing)
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Funcy.Interpreter.Modules where

import Control.Applicative
import Data.List
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set

data Containment c t = Containment {
    wrap :: t -> c,
    unwrap :: c -> Maybe t
}

chainCont :: Containment c b -> Containment b a -> Containment c a
chainCont con1 con2 = Containment {
    wrap = wrap con1 . wrap con2,
    unwrap = \x -> unwrap con1 x >>= unwrap con2
}

containLeft :: Containment (Either a b) a
containLeft = Containment {
    wrap = \u -> Left u,
    unwrap = \v -> case v of
        Left u -> Just u
        _ -> Nothing
}

containRight :: Containment (Either a b) b
containRight = Containment {
    wrap = \u -> Right u,
    unwrap = \v -> case v of
        Right u -> Just u
        _ -> Nothing
}


class Expansive f where
    expandWith :: Containment b a -> f a -> f b

newtype Prop c f a = Prop { prop :: c -> f a }
instance (Expansive f) => Expansive (Prop c f) where
    expandWith cont expEv = Prop (expandWith cont . prop expEv)

class Broad a where
    evaluateBroad :: Expansive f => Containment c a -> f c

instance (Expansive f) => Expansive (Broad f) where
    expandWith cont expEv = \broadCont -> (broad expEv $ chainCont broadCont cont)




type Domain = String

-- Set of direct dependencies
type DirectDeps = Set.Set Domain

-- Mapping which maps each indirect dependency module to specific direct dependency module which is dependent to the indirect module.
type DepDistrib = Map.Map Domain Domain

-- Dependencies for moduloid m. Here m represents the sum type of supported types.
data Dependencies m = DepInfo DirectDeps DepDistrib | Dummy m

instance Expansive Dependencies where
    expandWith _ (Deps a b) = Deps a b
    expandWith cont (Dummy m) = Dummy $ wrap cont m


-- Moduloid, which is basically a group of modules
class Moduloid l where
    dependencies :: Dependencies l


data Module m dep = Main m | Deps dep
data ModuleInstance dep = Instance

:: ModuleInstance dep -> f dep

class ModuleName m where
    default :: Maybe m
    moduleName :: Maybe m -> Domain

instance (ModuleName m, Moduloid dep) => Moduloid (Module m dep) where
    dependencies = DepInfo (this) Map.singleton (getDeps dependencies)

-- A module


getDirectDep :: Dependencies m -> Domain -> Maybe (Dependencies m)
getDirectDep (Deps _ distrib) domain = fmap ModuleMain $ Map.lookup domain distrib
getDirectDep _ _ = error "Wow, dummy? Really?"

isDirectDep :: Dependencies m -> Domain -> Bool
isDirectDep (Deps dirs _) domain = Set.member domain dirs
isDirectDep _ _ = error "Wow, dummy? Really?"

combineDeps :: Dependencies l -> Dependencies r -> Dependencies (Either l r)
combineDeps x y = error "For now, error."


liftFeatureL :: Expansive f => Dependencies l -> Dependencies (Either l r) -> Maybe (f l) -> Maybe (f (Either l r))
liftFeatureL deps directDep fLeft = do
    drDepDom <- extractDomain directDep
    if isDirectDep deps drDepDom then fmap (expandWith containLeft) fLeft else Nothing

liftFeatureR :: Expansive f => Dependencies r -> Dependencies (Either l r) -> Maybe (f r) -> Maybe (f (Either l r))
liftFeatureR deps directDep fRight = do
    drDepDom <- extractDomain directDep
    if isDirectDep deps drDepDom then fmap (expandWith containRight) fRight else Nothing

    featureFor domain = do
        dirDom <- getDirectDep dependencies domain
        (liftFeatureL dependencies dirDom $ featureFor domain) <|> (liftFeatureR dependencies dirDom $ featureFor domain)

instance (Moduloid l, Moduloid r) => Moduloid (Either l r) where
    dependencies = combineDeps dependencies dependencies


