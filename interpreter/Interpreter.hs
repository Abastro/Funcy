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

--class Exchangeable e where
--    exchange :: Name -> e

class LambdaLike l where
    evaluate :: l -> [l] -> Desc l
    substitute :: Map.Map Name l -> l -> Desc l

instance LambdaLike Void where
    evaluate x y = x
    substitute m p = p

data Lambda value = Value value | Ref Name [Lambda value] | Lam Name (Lambda value)

instance (LambdaLike expr) => LambdaLike (Lambda expr) where
    evaluate (Lam pName content) vals = substitute (Map.singleton pName vals) content
    evaluate (Ref pName list) vals = Ref pName (list vals)

    substitute rules (Lam pName content) = if collision then describeError "Re-declared" else Lambda pName (expand rules content)
    substitute rules (Ref pName list) = maybe (Ref pName list) (evaluate t list) ( pName rules)


class FPLike fp where
    pair :: fp -> fp -> Desc fp
    extract :: fp -> Desc fp
    apply :: fp -> Desc fp

data FuncPair ext = FPExt ext | Func (LFP ext) | Pair (LFP ext) ext
type LFP ext = Lambda (FuncPair ext)

instance (LambdaLike ext) => LambdaLike (FuncPair ext) where
    evaluate _ _ = describeError ""

    substitute rules (FPExt other) = FPExt (substitute rules other)
    substitute rules (Func lambda) = Func (substitute rules lambda)
    substitute rules (Pair lambda dep) = Pair (substitute rules lambda) (substitute rules dep)

instance (FPLike ext) => FPLike LFP ext where
    pair (Value value) _ = describeError ""
    pair lam dep = Value (Pair lam dep)

    extract (Value val) = extract val
    extract _ = describeError ""

    apply (Pair lambda dep) = evaluate lambda [dep]
    apply _ = describeError ""

classify :: Lambda Void -> LFP Void
classify (Lam pName content) = Lam pName (classify content)
classify (Ref pName list) = 


class Typed tp where
    typeOf :: tp -> tp

data PSFlag = PiType | SigmaType
data Types ext = TypeUni Int | PiSigma PSFlag (Types ext) (LT ext) | CustomType ext
type LT ext = Lambda (Types ext)

instance Typed (Typed ext) where
    typeOf (TypeUni n) = TypeUni (n + 1)
    typeOf _ = TypeUni 0

data Clause expr = Type (Types expr) | TypedExpr (Clause expr) expr

instance Typed Clause expr where
    typeOf (Type t) = typeOf t
    typeOf (TypedExpr t raw) = t

instance (LambdaLike expr) => LambdaLike Clause expr where
    evaluate _ _ = describeError ""
    substitute rules (Type t) = Type (substitute rules t)
    substitute rules (TypedExpr t raw) = TypedExpr (substitute rules t) (substitute rules raw)

instance (FuncPair expr) => FuncPair Clause expr where
    pair (TypedExpr cl ex) = 


data BlockN expr = Empty | BlockS (Block expr) (Name, expr)

data ExPair expr = ExPair expr expr

instance Pair (ExPair expr) expr where
    extract (ExPair ev dep) = pure $ dep
    funcIn (ExPair ev dep) = pure $ ev

instance (Expandable expr) => Expandable (ExPair expr) expr where
    expand rules (ExPair ev dep) = ExPair (expand rules ev) (expand rules dep)


data Operate expr = Apply expr | Extract expr

instance (Function expr expr, Pair expr expr, Expandable expr) => Expandable (Operate expr) expr where
    expand rules (Apply content) = let pair = expand rules content
        in apply (funcIn pair) (extract pair)
    expand rules (Extract content) = extract (expand rules content)


data Ref = Ref name

instance Expandable Ref expr where
    expand rules (Ref name) = lookup rules name

data Basis expr = FromLambda (Lambda expr) | FromExPair (ExPair expr) | FromOperation (Opearate expr) | FromRef Ref

instance (Function expr expr, Pair expr expr, Expandable expr) => Expandable (Operate expr) expr where
    expand rules (FromLambda l) = FromLambda (expand rules l)
    expand rules (FromExPair p) = FromExPair (expand rules p)
    expand rules (FromOperation opn) = FromOperation (expand rules opn)
    expand rules (FromRef ref) = FromRef (expand rules ref)

-- Typed description


-- Evaluation process
data Extern = ExtName Name


type ID = Name
data EvalMapping = EL ID Evaluated | EB (Map.Map Name Evaluated) deriving Show
data EvalConnect = EC Evaluated Evaluated deriving Show

data EvalContext = DepRef ID | ExtEv Extern | EF EvalMapping | EP EvalConnect deriving Show

data Evaluated = ExEval EvalContext | ImEval Evaluated Operation deriving Show
type Intermediate = (Evaluated, Set.Set ID)

evaluate :: Clause -> Maybe Intermediate
evaluate clause = case clause of
    Implicit param op -> do
        (evSub, holes) <- evaluate param
        ev <- case op of
            Apply -> apply evSub
            Extract -> extract evSub
            _ -> return $ ImEval evSub op
        return $ (ev, holes)
    Explicit _ context -> do
        (evContext, holes) <- case context of
            IntRef name -> return (DepRef name, Set.singleton name)
            ExtRef name -> return (ExtEv $ ExtName name, Set.empty) -- This is external, anyway
            CFunc mapping -> case mapping of
                Lambda name expr -> do
                    (evSub, holeSub) <- evaluate expr
                    return (EF $ EL name evSub, Set.delete name holeSub)
                Block map -> do
                    blockContent <- foldl comb (return Map.empty) map
                    return (EF $ EB $ Map.map fst blockContent, foldl Set.union Set.empty $ Map.map snd blockContent)
            CPair (Con fn dep) -> do
                (evFn, fnh) <- evaluate fn
                (evDep, deph) <- evaluate dep
                return (EP $ EC evFn evDep, Set.union fnh deph)
        return (ExEval evContext, holes)

comb :: Maybe (Map.Map Name Intermediate) -> (Name, Clause) -> Maybe (Map.Map Name Intermediate)
comb prev (name, expr) = do
    stored <- prev
    (evCur, ids) <- evaluate expr
    evEnd <- let subs bid acc = maybe acc ((>>=) acc) $ fmap (substitute bid) (fmap fst $ Map.lookup bid stored)
        in foldr subs (return evCur) ids
    let idsAfter = foldl Set.union Set.empty $ Set.map (\someId -> maybe (Set.singleton someId) snd $ Map.lookup someId stored) ids
        in let subs2 (evSub, holes) = fmap (,) (substitute name evEnd evSub) >>= pure . flip ($) (Set.union idsAfter $ Set.delete name holes)
            in traverse subs2 (Map.insert name (evEnd, idsAfter) stored)


apply :: Evaluated -> Maybe Evaluated
apply pair = do
    EC ev dep <- disassemblePair pair
    case ev of
        ExEval context -> case context of
            EF mapping -> case mapping of
                EL did expr -> substitute did dep expr
                EB map -> case dep of
                    ExEval shouldBeName -> case shouldBeName of
                        ExtEv maybeName -> case maybeName of
                            ExtName name -> Map.lookup name map
                            _ -> Nothing
                        _ -> Nothing
                    ImEval _ _ -> Nothing
            DepRef _ -> return $ ImEval pair Apply
            _ -> Nothing
        ImEval intern op -> case op of
            Apply -> return $ ImEval pair Apply
            Extract -> return $ ImEval pair Apply
            _ -> Nothing

extract :: Evaluated -> Maybe Evaluated
extract pair = fmap ext $ disassemblePair pair where
    ext (EC fn dep) = dep

disassemblePair :: Evaluated -> Maybe EvalConnect
disassemblePair (ExEval context) = case context of
    EP connect -> Just connect
    _ -> Nothing
disassemblePair _ = Nothing

-- Map based version?
substitute :: ID -> Intermediate -> Evaluated -> Maybe Intermediate
substitute did (dep, holeDep) expr = case expr of
    ExEval context -> case context of
        DepRef rid -> return $ if did == rid then (dep, holeDep) else (expr, Set.singleton rid)
        ExtEv _ -> return expr
        EF mapping -> fmap (ExEval . EF) $ case mapping of
            EL otherId evaluated -> if otherId == did then Nothing else fmap (EL otherId) $ subsSame evaluated
            EB map -> fmap EB $ traverse subsSame map
        EP connect -> fmap (ExEval . EP) $ case connect of
            EC func depen -> liftA2 EC (subsSame func) (subsSame depen)
    ImEval inter op -> case op of
        Apply -> subsSame inter >>= \app -> apply $ ImEval app op
        Extract -> subsSame inter >>= \ext -> extract $ ImEval ext op
        _ -> return expr
    where subsSame = substitute did (dep, holeDep)