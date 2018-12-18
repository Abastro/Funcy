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


chainCont :: Containment b a -> Containment c b -> Containment c a
chainCont con2 con1 = Containment {
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


newtype Prop f c a = Prop { deProp :: c -> f a }
instance (Expansive f) => Expansive (Prop f c) where
    expandWith cont toFeature = Prop (expandWith cont . deProp toFeature)

newtype Broad f c a = Broad { deBroad :: Containment c a -> f c }
instance Expansive (Broad f c) where
    expandWith cont toFeature = Broad (deBroad toFeature . chainCont cont)


type Domain = String

-- Set of direct dependencies
type DirectDeps = Set.Set Domain

-- Mapping which maps each indirect dependency module to specific direct dependency module which is dependent to the indirect module.
type DepDistrib = Map.Map Domain Domain

-- Dependencies for moduloid m. Here m represents the sum type of supported types.
data Dependencies dep = DepInfo DirectDeps DepDistrib


-- Moduloid, which is basically a group of modules
class Moduloid l where
    dependencies :: Dependencies l

combineDeps :: Dependencies l -> Dependencies r -> Dependencies (Either l r)
combineDeps x y = error "For now, error."    

instance (Moduloid l, Moduloid r) => Moduloid (Either l r) where
    dependencies = combineDeps dependencies dependencies
    

-- A type which can represent all types a module requires
data ModuleType loc dep = Local loc | Deps dep

-- Module Instance
data ModuleInstance loc = MInstance Domain

-- ModuleLocal class, which represents the local type
class ModuleLocal loc where
    mInstance :: ModuleInstance loc

createDep :: Dependencies dep -> ModuleInstance loc -> Dependencies (ModuleType loc dep)
createDep (DepInfo _ depDist) (MInstance domain) = DepInfo (Set.singleton domain) (foldr Map.union Map.empty $ fmap (\anyDeps -> Map.singleton anyDeps domain) depDist)

instance (ModuleLocal loc, Moduloid dep) => Moduloid (ModuleType loc dep) where
    dependencies = createDep dependencies mInstance


getDirectDep :: Dependencies dep -> Domain -> Maybe (ModuleInstance dep)
getDirectDep (DepInfo _ distrib) domain = fmap MInstance $ Map.lookup domain distrib

isDirectDep :: Dependencies dep -> Domain -> Bool
isDirectDep (DepInfo dirs _) domain = Set.member domain dirs


type DepFeature f dep = (Dependencies dep, Maybe (f dep))

liftFeatureL :: (Expansive f, Moduloid (Either l r)) => DepFeature f l -> ModuleInstance (Either l r) -> Maybe (f (Either l r))
liftFeatureL (deps, fLeft) (MInstance dirDomain) =
    if isDirectDep deps dirDomain
        then fmap (expandWith containLeft) fLeft
        else Nothing

liftFeatureR :: (Expansive f, Moduloid (Either l r)) => DepFeature f r -> ModuleInstance (Either l r) ->  Maybe (f (Either l r))
liftFeatureR (deps, fRight) (MInstance dirDomain) =
    if isDirectDep deps dirDomain
        then fmap (expandWith containRight) fRight
        else Nothing


liftFeature :: (Expansive f, Moduloid (Either l r)) => Domain -> DepFeature f l -> DepFeature f r -> DepFeature f (Either l r)
liftFeature domain dfLeft dfRight = (,) dependencies $ do
    dirDomain <- getDirectDep dependencies domain
    liftFeatureL dfLeft dirDomain <|> liftFeatureR dfRight dirDomain


class (Expansive f, Moduloid l) => ModuleFeature f l where
    queryFeature :: Domain -> Maybe (f l)

-- Module feature of Union
instance (ModuleFeature f l, ModuleFeature f r) => ModuleFeature f (Either l r) where
    queryFeature domain = snd $ liftFeature domain (dependencies, queryFeature domain) (dependencies, queryFeature domain)
