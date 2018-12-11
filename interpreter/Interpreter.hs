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

class Lambda l where
    evaluate :: l -> l -> Desc l

class Expandable a m | a -> m where
    expand :: Map.Map Name 

data Lambda = Ref Int | CLambda Int Lambda

instance Lambda LambdaCalc where
    apply (CLambda pName content) val = expand (Map.singleton pName val) content
    apply fn val = Subs fn val

instance Expandable LambdaCalc where
    expand rules (Lambda pName content) = if collision then describeError "Re-declared" else Lambda pName (expand rules content)

Basis = DepFunc LambdaCalc | DepPair LambdaCalc Basis | 

class Pair p m | p -> m where
    first :: p -> Desc m
    second :: p -> Desc m

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

data Clause = TypeUni Int | TypedExpr Clause (Expr Clause)

class Typed tp where
    typeOf :: tp -> tp

instance Typed Clause where
    typeOf (TypeUni n) = TypeUni (n + 1)
    typeOf (Explicit tp raw) = tp

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