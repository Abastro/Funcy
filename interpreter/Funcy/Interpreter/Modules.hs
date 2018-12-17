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

instance (Functor f) => Expansive f where
    expandWith cont = fmap wrap

newtype Prop f c a = Prop { prop :: c -> f a }
instance (Expansive f) => Expansive (Prop f c) where
    expandWith cont toFeature = Prop (expandWith cont . prop toFeature)

newtype Broad f c a = Broad { broad :: Containment c a -> f a }
instance (Expansive f) => Expansive (Broad f c) where
    expandWith cont toFeature = Broad (expandWith cont . broad toFeature . chainCont cont)


type Domain = String

-- Set of direct dependencies
type DirectDeps = Set.Set Domain

-- Mapping which maps each indirect dependency module to specific direct dependency module which is dependent to the indirect module.
type DepDistrib = Map.Map Domain Domain

-- Dependencies for moduloid m. Here m represents the sum type of supported types.
data Dependencies dep = DepInfo DirectDeps DepDistrib

--instance Expansive Dependencies where
--    expandWith _ (Deps a b) = Deps a b


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
data ModuleInstance m = MInstance Domain

-- Module class
class Module m where
    mInstance :: ModuleInstance m

createDep :: Dependencies dep -> ModuleInstance (ModuleType loc dep) -> Dependencies (ModuleType loc dep)
createDep (DepInfo _ depDist) (MInstance domain) = DepInfo (Set.singleton domain) (Map.singleton domain $ Map.keys depDist)

instance (Module (ModuleType loc dep), Moduloid dep) => Moduloid (ModuleType loc dep) where
    dependencies = createDep dependencies mInstance


getDirectDep :: Dependencies dep -> Domain -> ModuleInstance dep
getDirectDep (Deps _ distrib) domain = fmap MInstance $ Map.lookup domain distrib

isDirectDep :: Dependencies dep -> Domain -> Bool
isDirectDep (Deps dirs _) domain = Set.member domain dirs

type DepFeature f dep = (Dependencies dep, Maybe (f dep))

liftFeatureL :: (Expansive f, Moduloid (Either l r)) => DepFeature f l -> ModuleInstance (Either l r) -> DepFeature (f (Either l r))
liftFeatureL (deps, fLeft) (MInstance dirDomain) =
    if isDirectDep deps dirDomain
        then (dependencies, fmap (expandWith containLeft) fLeft)
        else (dependencies, Nothing)

liftFeatureR :: (Expansive f, Moduloid (Either l r))=> DepFeature f r -> ModuleInstance (Either l r) -> DepFeature (f (Either l r))
liftFeatureR (deps, fRight) (MInstance dirDomain) =
    if isDirectDep deps dirDomain
        then (dependencies, fmap (expandWith containRight) fRight)
        else (dependencies, Nothing)

liftFeature :: (Expansive f, Moduloid (Either l r)) => Domain -> DepFeature f l -> DepFeature f r -> DepFeature f (Either l r)
liftFeature domain dfLeft dfRight = do
    dirDomain <- getDirectDep dependencies domain
    (liftFeatureL dependencies dirDomain dLeft) <|> (liftFeatureR dependencies dirDomain dRight)

