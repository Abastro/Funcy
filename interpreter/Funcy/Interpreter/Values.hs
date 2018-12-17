{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Funcy.Interpreter.InterModules where

import Control.Applicative
import Data.List
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Funcy.Interpreter.Modules

type Name = String


-- Class Description
class (Monad d) => Description d where
    describeError :: String -> d a

newtype Desc a = Desc (Maybe a) deriving (Functor, Applicative, Monad)

instance Description Desc where
    describeError msg = Desc Nothing



data Applier ext = Applier {
    -- Check for errors on application
    checkApply :: [Maybe ext] -> Desc (),

    -- Performs actual application
    performApply :: [Maybe ext] -> Maybe ext
}

instance (Functor f, Functor g) => f g where
    

instance Expansive Applier where
    expandWith cont applier = Applier {
        checkApply = (checkApply applier) . (fmap $ flip (>>=) $ unwrap cont)
    }


{-
expandApplier :: Contained b a => Applier a -> Applier b
expandApplier applier = Applier {
    checkApply = (checkApply applier) . (fmap $ flip (>>=) unwrap),
    performApply = (fmap wrap) . (performApply applier) . (fmap $ flip (>>=) $ unwrap)
}

-}

data FuncPair ext = Pair Bool (FuncPair ext) (FuncPair ext) |
    Extract (FuncPair ext) | Apply (FuncPair ext) |
    Extern ext

{-
-- Sum type of types introduced in this module
class InterModule m where
    -- Sum type of all dependency modules, with m itself included
    data Deps m
    moduleInfo :: ModuleInfo (Deps m)
    getModifier :: (Containment ex (Deps m)) -> Name -> Applier (FuncPair ex)
    --requiredParams :: Name -> Maybe Int
    --getApplier :: ModuleInfo m -> Name -> Applier (Deps m)


class ModuleGroup deps where
    queryApplier :: Domain -> Name -> Applier (FuncPair deps)
    queryModifier :: (Containment ex deps) -> Domain -> Name -> Applier (FuncPair ex)



instance ModuleFeature 
-}