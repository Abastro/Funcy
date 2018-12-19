-- Module System for Funcy (Most of this is just to simulate duck typing)
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Funcy.Interpreter.Modules where

import Control.Applicative
import Data.List
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set

data Containment c t = Containment {
    wrap :: t -> c,
    unwrap :: c -> Maybe t
}

idCont :: Containment a a
idCont = Containment {
    wrap = id,
    unwrap = Just
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

{-
instance Expansive Identity where
    expandWith cont = Identity . wrap cont . runIdentity
-}

{-
newtype Prop f c a = Prop { deProp :: c -> f a }
instance (Expansive f) => Expansive (Prop f c) where
    expandWith cont toFeature = Prop (expandWith cont . deProp toFeature)
-}

newtype Broad f c a = Broad { deBroad :: Containment c a -> f c }
instance Expansive (Broad f c) where
    expandWith cont toFeature = Broad (deBroad toFeature . chainCont cont)


-- A type which can represent all types a module requires
-- Type dep consists of the direct dependency module types
newtype ModuleType loc dep = Modulize { asRawTyped :: Either loc dep }

moduleWraps :: Containment (ModuleType loc dep) (Either loc dep)
moduleWraps = Containment {
    wrap = Modulize,
    unwrap = Just . asRawTyped
}

type Domain = String

-- Set of direct dependencies
type DirectDeps = Set.Set Domain

-- Mapping which maps each indirect dependency module to specific direct dependency module which is dependent to the indirect module.
type DepDistrib = Map.Map Domain Domain

-- Dependencies for moduloid m. Here m represents the sum type of supported types.
data Dependencies dep = DepInfo DirectDeps DepDistrib


-- Domained group
class DomainedGroup g where
    dependencies :: Dependencies g

-- Combines dependencies.
-- TODO : More efficient approach
combineDeps :: Dependencies l -> Dependencies r -> Dependencies (Either l r)
combineDeps (DepInfo dir1 distrib1) (DepInfo dir2 distrib2) = DepInfo (Set.union dir1 dir2) (Map.union distrib1 distrib2)

instance (DomainedGroup l, DomainedGroup r) => DomainedGroup (Either l r) where
    dependencies = combineDeps dependencies dependencies

data Void

instance DomainedGroup Void where
    dependencies = DepInfo Set.empty Map.empty


-- Representation of Module Domain
data ModuleDomain loc = MInstance Domain

-- DomainedLocal class, which represents the local type and gives domain
class DomainedLocal loc where
    mInstance :: ModuleDomain loc


createDep :: Dependencies dep -> ModuleDomain loc -> Dependencies (ModuleType loc dep)
createDep (DepInfo _ depDist) (MInstance domain) = DepInfo (Set.singleton domain) (foldr Map.union Map.empty $ fmap (\anyDeps -> Map.singleton anyDeps domain) depDist)

instance (DomainedLocal loc, DomainedGroup dep) => DomainedGroup (ModuleType loc dep) where
    dependencies = createDep dependencies mInstance


-- Features
class (Expansive f, DomainedGroup g) => ElementFeature f g where
    featureOf :: g -> f g

class (Functor f, DomainedLocal loc) => LocalFeature f loc dep where
    getFeature :: f (ModuleType loc dep)

class (Functor f, DomainedGroup g) => DomainedFeature f g where
    findFeature :: Domain -> Maybe (f g)


instance (Expansive f) => ElementFeature f Void where
    featureOf _ = error "The heck is this"

instance (Functor f) => DomainedFeature f Void where
    findFeature _ = Nothing



instance (ElementFeature f l, ElementFeature f r) => ElementFeature f (Either l r) where
    featureOf elem = case elem of
        Left el -> expandWith containLeft $ featureOf el
        Right el -> expandWith containRight $ featureOf el


getDirectDep :: Dependencies dep -> Domain -> Maybe (ModuleDomain dep)
getDirectDep (DepInfo _ distrib) domain = fmap MInstance $ Map.lookup domain distrib

findFeatureDep :: DomainedFeature f dep => Dependencies dep -> Domain -> Maybe (f dep)
findFeatureDep (DepInfo dirs _) domain = if Set.member domain dirs then findFeature domain else Nothing

expandFeature :: DomainedFeature f g => ModuleDomain h -> (g -> h) -> Maybe (f h)
expandFeature (MInstance domain) trans = (fmap $ fmap trans) (findFeatureDep dependencies domain)

-- Domained feature of Union
instance (DomainedFeature f l, DomainedFeature f r) => DomainedFeature f (Either l r) where
    findFeature domain = do
        dirDomain <- getDirectDep dependencies domain
        expandFeature dirDomain Left <|> expandFeature dirDomain Right


instance (DomainedLocal loc, ElementFeature f loc, ElementFeature f dep) => ElementFeature f (ModuleType loc dep) where
    featureOf elem = case asRawTyped elem of
        Left el -> expandWith moduleWraps $ expandWith containLeft $ featureOf el
        Right el -> expandWith moduleWraps $ expandWith containRight $ featureOf el

instance (LocalFeature f loc dep, DomainedFeature f dep) => DomainedFeature f (ModuleType loc dep) where
    findFeature domain = (featureInModule domain mInstance) <|> ( fmap $ fmap (Modulize . Right) ) (findFeature domain)

featureInModule :: LocalFeature f loc dep => Domain -> ModuleDomain loc -> Maybe (f (ModuleType loc dep))
featureInModule domain (MInstance domM) = if domain == domM then Just getFeature else Nothing


