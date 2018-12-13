-- Basis, Block / Lambda / Dependent Type
import Control.Applicative
import Data.List
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set

type Domain = String
type Name = String

-- Class Description
class (Monad d) => Description d where
    describeError :: String -> d

type Desc = Maybe
instance Description Desc where
    error msg = Nothing


class Contained c t | t -> c where
    wrap :: t -> c
    unwrap :: c -> Maybe t


-- Sum type of types introduced in this module
class InterModule m where
    -- Sum type of all dependency modules, with m itself included
    data AllDeps m
    modName :: m -> String



data FuncPair ext = Extern ext | Pair (FuncPair ext) (FuncPair ext) | Extract (FuncPair ext) | Apply (FuncPair ext)


evaluateBase :: FuncPair ext -> FuncPair ext
evaluateBase (Apply (Pair fn dep))

-- Module Frame.Name

type FrameName = Name

instance InterModule FrameName where
    data AllDeps FrameName = FName_Self FrameName



-- Module Frame.Lambda

data FrameLambda = Lambda Name

instance InterModule FrameLambda where
    data AllDeps FrameLambda = FLambda_FName FrameName | FLambda_Self FrameLambda

modify :: (Contained m FrameLambda) => Name -> [FuncPair m] -> FuncPair m


-- Module Std.Integers

data Integers = BigInt Integer | I64 Int

instance InterModule Integers where
    data AllDeps Integers = StdInts_FName Name | StdInts_Self Integers

type DepInts = AllDeps Integers

evaluate :: FuncPair DepInts -> FuncPair DepInts


apply :: Name -> [FuncPair DepInts] -> FuncPair DepInts


data PSFlag = Pi | Sigma
data DepType = TypeUni Int | Depend PSFlag Types (Lambda Types) | ExtType Name

data Clause ext = Typer DepType | Typed DepType (FuncPair ext)

