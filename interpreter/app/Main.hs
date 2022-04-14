-- Basis, Block / Lambda / Dependent Type
import Funcy.Interpreter.Modules
import Funcy.Interpreter.InterModules

{-
Primitive.Integral.Add 3 -> Add, [3]
Primitive.Integral.3 -> 3 [] -> 3

Pair False 3 (+)
Pair True 3 (3+)


-- Module Frame.ApplyChain

type FrameApplyChain ext = ApplyChain Name [ext]

instance InterModule FrameApplyChain where
    data Deps FrameApplyChain = FName_Self FrameApplyChain

evaluate :: (Contained ) => FuncPair (Domain, Name) -> FuncPair ext
evaluate (Extract pair) = case evaluate pair of
    Pair _ ext _ -> ext
    other -> Extract other
evaluate (Apply pair) = case evaluate pair of
    Pair True _ app -> app
    other -> Apply other
evaluate (Extern (domain, name)) = getModifier $ domain name
evaluate (Pair False dep fn) = 




-- Module Frame.Lambda

data FrameLambda = LamRef Name

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

-}