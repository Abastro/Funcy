-- Basis, Block / Lambda / Dependent Type
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
            _ -> fmap ImEval evSub op
        return $ (ev, holes)
    Explicit _ context -> do
        (evContext, holes) <- case context of
            IntRef name -> (ExEval $ DepRef name, Set.singleton name)
            ExtRef name -> (ExEval $ ExtName name, Set.empty) -- This is external, anyway
            CFunc mapping -> case mapping of
                Lambda name expr -> do
                    (evSub, holeSub) <- evaluate expr
                    (EL name evSub, Set.delete name holeSub)
                Block list -> foldl comb (Just Map.empty) list
            CPair (Con fn dep) -> do
                (evFn, fnh) <- evaluate fn
                (evDep, deph) <- evaluate dep
                return (EP $ EC evFn evDep, Set.union fnh deph)
        return (ExEval evContext, holes)

comb :: Maybe (Map.Map Name Intermediate) -> (Name, Clause) -> Maybe (Map.Map Name Intermediate)
comb prev (name, expr) = do
    stored <- prev
    (evCur, ids) <- evaluate clause
    --evEnd <- foldr subs (return evCur) ids where
    --    subs bid = liftA2 (substitute bid) (fmap fst (stored Map.!? bid))
    idsAfter <- unions $ fmap (\someId -> if member someId stored then snd $ stored Map.! someId else Set.singleton someId) ids
    if member name idsAfter
        then Nothing
        else traverse subs2 (Map.insert name evEnd stored) where
            subs2 :: Intermediate -> Maybe Intermediate
            subs2 (evSub, holes) = fmap (,) (substitute name evEnd evSub) (Set.union (Set.delete name holes) idsAfter)


apply :: Evaluated -> Maybe Evaluated
apply pair = disassemblePair pair >>= \conn ->
    case ev of
        ExEval context -> case context of
            EF mapping -> case mapping of
                EL did expr -> return $ substitute did dep expr
                EB list -> case dep of
                    ExEval shouldBeName -> case shouldBeName of
                        ExtEv maybeName -> case maybeName of
                            ExtName name -> return $ snd (find (\x -> fst x == name) list)
                            _ -> Nothing
                        _ -> Nothing
                    ImEval _ _ -> Nothing
            DepRef _ -> return $ ImEval pair Apply
            _ -> Nothing
        ImEval intern op -> case op of
            Apply -> return $ ImEval pair Apply
            Extract -> return $ ImEval pair Apply
            _ -> Nothing
    where (ev, dep) = conn

extract :: Evaluated -> Maybe Evaluated
extract pair = fmap snd $ disassemblePair pair

disassemblePair :: Evaluated -> Maybe EvalConnect
disassemblePair (ExEval context) = case context of
    EP connect -> Just connect
    _ -> Nothing
disassemblePair _ = Nothing

-- Map based version?
substitute :: ID -> Evaluated -> Evaluated -> Maybe Evaluated
substitute did dep expr = case expr of
    ExEval context -> case context of
        DepRef rid -> if did == rid then dep else expr
        ExtEv _ -> expr
        EF mapping -> case mapping of
            EL otherId evaluated -> if otherId == id then Nothing else fmap (EL otherId) $ subsSame evaluated
            EB list -> fmap EB $ traverse (\v -> (fst v, subsSame $ snd v)) list
        EP connect -> case connect of
            EC func depen -> liftA2 EC (subsSame func) (subsSame depen)
    ImEval inter op -> case op of
        Apply -> fmap (apply. ImEval) $ subsSame inter
        Extract -> fmap (extract. ImEval) $ subsSame inter
        _ -> return expr
    where subsSame = substitute id dep