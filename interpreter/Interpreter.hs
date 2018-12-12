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
    typeName :: t -> Name
    wrap :: t -> c
    unwrap :: c -> Maybe t


class InterModule m where
    modName :: m -> String



-- Untyped

data FuncPair ext = Extern ext | Pair (FuncPair ext) (FuncPair ext) | Extract (FuncPair ext) | Apply (FuncPair ext)

class Modifier c mod | mod -> c where
    modify :: mod -> FuncPair c -> FuncPair c

newtype Unary c = Unary {
    transform :: c -> c
}

instance (Modifier c) (Unary c) where
    modify mod (Extern par) = Extern $ transform mod par


interpret :: FuncPair Name -> FuncPair c






data PSFlag = Pi | Sigma
data DepType = TypeUni Int | Depend PSFlag Types (Lambda Types) | ExtType Name

data Clause ext = Typer DepType | Typed DepType (FuncPair ext)


impSimple :: Name -> a -> Clause a
impSimple tName ext = Typed (Extern tName) (External ext)


instance Functor Clause where
    map conv cl = (conv p) -- (Here comes all the application, but Idk how)

instance Applicative Clause where
    (<*>) clConv clDep = 