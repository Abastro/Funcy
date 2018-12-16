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
data Dependencies m = Deps DirectDeps DepDistrib | ModuleMain Domain | Dummy m

instance Expansive Dependencies where
    expandWith _ (Deps a b) = Deps a b
    expandWith _ (ModuleMain d) = ModuleMain d
    expandWith cont (Dummy m) = Dummy $ wrap cont m


-- Moduloid, which is basically a group of modules
class Moduloid l where
    dependencies :: Dependencies l
    featureFor :: Expansive f => Domain -> Maybe (f l)


getDirectDep :: Dependencies m -> Domain -> Maybe (Dependencies m)
getDirectDep (Deps _ distrib) domain = fmap ModuleMain $ Map.lookup domain distrib
getDirectDep (ModuleMain moduleName) domain = if moduleName == domain then Just (ModuleMain domain) else Nothing
getDirectDep _ _ = error "Wow, dummy? Really?"

isDirectDep :: Dependencies m -> Domain -> Bool
isDirectDep (Deps dirs _) domain = Set.member domain dirs
isDirectDep (ModuleMain moduleName) domain = (moduleName == domain)
isDirectDep _ _ = error "Wow, dummy? Really?"

extractDomain :: Dependencies m -> Maybe Domain
extractDomain (ModuleMain domain) = Just domain
extractDomain _ = Nothing

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


instance (Moduloid l, Moduloid r) => Moduloid (Either l r) where
    dependencies = combineDeps dependencies dependencies
    featureFor domain = do
        dirDom <- getDirectDep dependencies domain
        (liftFeatureL dependencies dirDom $ featureFor domain) <|> (liftFeatureR dependencies dirDom $ featureFor domain)

