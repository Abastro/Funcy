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

data Void

class Mapping m where
    mapping :: m -> m -> Desc m

class (Mapping ev) => Evaluable ev where
    replace :: Int -> ev -> ev -> Desc ev


data Lambda expr = Val expr | Ref Int [Lambda expr] | Lam Int (Lambda expr)

instance (Evaluable expr) => Mapping Lambda expr where
    mapping ev dep = (evalLambda ev dep : [])

instance (Evaluable expr) => Evaluable Lambda expr where
    replace iref dep (Ref r pars) =
        let postP = map (replace iref dep) list
        in if iref == r then evalLambda dep postP else Ref r postP
    replace iref dep (Lam r exp) =
        if iref == r
            then describeError "Declared Twice"
            else Lam r (replace iref dep exp)

evalLambda :: (Evaluable expr) => Lambda expr -> [Lambda expr] -> Desc (Lambda expr)
evalLambda (Lam ref sub) (val : rest) = evalLambda (replace ref val sub)
evalLambda (Ref r list) (v : vals) = Ref r (list (v : vals))
evalLambda l [] = l


data FuncPair ext = External ext | FPL (Lambda (FuncPair ext)) | Pair (FuncPair ext) (FuncPair ext) | Extract (FuncPair ext) | Apply (FuncPair ext)


instance Mapping FuncPair ext where
    mapping (FPL lambda) dep = ...


pair :: (Mapping ext) => FuncPair ext -> FuncPair ext -> FuncPair ext
pair fun dep = Pair fun dep

extract :: FuncPair -> FuncPair
extract (Pair fun dep) = dep
extract pair = Extract pair

apply :: FuncPair -> FuncPair
apply (Pair (External ext) dep) = mapping ext dep
apply (Pair (FPL lambda) dep) = mapping lambda dep
apply pair = Apply pair


data PSFlag = Pi | Sigma
data DepType = TypeUni Int | Depend PSFlag Types (Lambda Types) | Extern Name

data Clause ext = Typer DepType | Typed DepType (FuncPair ext)


impSimple :: Name -> a -> Clause a
impSimple tName ext = Typed (Extern tName) (External ext)


instance Functor Clause where
    map conv cl = (conv p) -- (Here comes all the application, but Idk how)

instance Applicative Clause where
    (<*>) clConv clDep = 