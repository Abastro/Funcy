-- Basis, Block / Lambda / Dependent Type
import Control.Applicative
import Data.List
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set

type Name = String

type Function = Clause

data Mapping = Lambda Name Clause | Block [(Name, Clause)]
data Connect = Con Function Clause

type Type = Clause

data Context = IntRef Name | ExtRef Name | CFunc Mapping | CPair Connect
data Operation = Apply | Extract | PiT | SigmaT

data Clause = Explicit (Maybe Type) Context | Implicit Clause Operation


-- Evaluation process
data Extern = Empty | ExtName Name

type ID = Name
data EvalMapping = EL ID Evaluated | EB (Map.Map Name Evaluated)
data EvalConnect = EC Evaluated Evaluated

data EvalContext = DepRef ID | ExtEv Extern | EF EvalMapping | EP EvalConnect

data Evaluated = ExEval EvalContext | ImEval Evaluated Operation
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
    evEnd <- let subs bid acc = do trans <- fmap (substitute bid) (fmap fst (Map.lookup bid stored)); acc >>= trans
        in foldr subs (return evCur) ids
    let idsAfter = foldl Set.union Set.empty $ Set.map (\someId -> maybe (Set.singleton someId) snd $ Map.lookup someId stored) ids
        in if Set.member name idsAfter
            then Nothing
            else let subs2 (evSub, holes) = fmap (,) (substitute name evEnd evSub) >>= pure . flip ($) (Set.union idsAfter $ Set.delete name holes)
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
substitute :: ID -> Evaluated -> Evaluated -> Maybe Evaluated
substitute did dep expr = case expr of
    ExEval context -> case context of
        DepRef rid -> return $ if did == rid then dep else expr
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
    where subsSame = substitute did dep