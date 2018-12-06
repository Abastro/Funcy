-- Basis, Block / Lambda / Dependent Type
import data.Map as Map

type Name = String

type Function = Clause

data Mapping = Lambda Name Clause | Block [(Name, Clause)]
data Connect = Con Function Clause

type Type = Clause

data Context = IntRef Name | ExtRef Name | CFunc Mapping | CPair Connect
data Operation = Apply | Extract | PiT | SigmaT

data Clause = Explicit (Maybe Type) Context | Implicit Clause Operation


-- Convert a clause into Address tree
type Address = String

data AddrMapping = AL Name Address | AB [(Name, Address)]
data AddrConnect = AC Address Address

data AddrContext = IR Name | ER Name | CF AddrMapping | CP AddrConnect

data ClauseDesc = ExDesc (Maybe Address) AddrContext | ImDesc Address Operation

type ClauseGraph = Map.Map Address ClauseDesc


asGraph = asGraphAddr ""

asGraphAddr :: Address -> Clause -> ClauseGraph
asGraphAddr base (Explicit tp ctx)  = Map.insert base (ExDesc (fmap (\_ -> addrTp) tp) $ fst ctxElem) $ Map.union (snd ctxElem) where
    ctxElem = asGraphElem base ctx
    addrTp = base ++ "$"

asGraphAddr base (Implicit cl op)   = Map.insert base (ImDesc addrCl op) $ asGraphAddr addrCl cl where
    addrCl = base ++ "#"

asGraphElem :: Address -> Context -> (AddrContext, ClauseGraph)
asGraphElem base (IntRef name)  = (IR name, Map.empty)
asGraphElem base (ExtRef name)  = (ER name, Map.empty)
asGraphElem base (CFunc mp)     = (CF $ fst mpg, snd mpg) where mpg = asGraphInMap base mp
asGraphElem base (CPair con)    = (CP $ fst cong, snd cong) where cong = asGraphInCon base con

asGraphInMap :: Address -> Context -> (AddrMapping, ClauseGraph)
asGraphInMap base (Lambda inp outp) = (AL inp addrOut, asGraphAddr addrOut outp) where
    addrOut = base ++ "." ++ inp

asGraphInMap base (block list) = ( AB $ map (\x -> (fst x, addrTrans x)) list, Map.unions $ map (\x -> asGraphAddr (addrTrans x) (snd x) ) list ) where
    addrTrans = (++) (base ++ ".") . fst

asGraphInCon :: Address -> Connect -> (AddrConnect, ClauseGraph)
asGraphInCon base (Con f c) = (AC addrFunc addrDept, Map.union (asGraphAddr addrFunc f) (asGraphAddr addrDept c) ) where
    addrFunc = base ++ "@"
    addrDept = base ++ "*"


-- Traverse the tree, fetching long-range link information
type Refs = [(Name, Address)]
type RefLinks = [(Address, Address)]
data Links = AsLink Refs RefLinks

findLink :: ClauseGraph -> Address -> Links
findLink graph addr = findLinkFor graph addr (graph ! addr)

findLinkFor :: ClauseGraph -> Address -> ClauseDesc -> Links
findLinkFor graph addr (ExDesc td cd) = 
findLinkFor graph addr (ImDesc saddr _) = findLink graph saddr


-- Cycles on value is not allowed

-- Typecheck
{-
typeCheck :: (ClauseGraph, Links) -> Address -> Mismatches
typeCheck (graph, lk) addr = typeCheckFor (graph, lk) addr (graph ! addr)

typeCheckFor :: (ClauseGraph, Links) -> Address -> ClauseDesc -> Mismatches
typeCheckIn (graph, lk) addr (ExDesc td cd) = 
-}

-- Computation (For now, preserve types purely for display)
data Extern = Empty | ExtName Name

type ID = Name
data EvalMapping = EL ID Evaluated | EB [(Name, Evaluated)]
data EvalConnect = EC Evaluated Evaluated

data EvalContext = DepRef ID | ExtEv Extern | EF AddrMapping | EP EvalConnect

data Evaluated = ExEval EvalContext | ImEval Evaluated Operation

-- $SomeName :: Enum "SomeName" enum => enum

evaluate :: ClauseGraph -> Address -> Evaluated
evaluate graph cur = evaluateFor graph (graph ! cur)

evaluateFor :: ClauseGraph -> ClauseDesc -> Evaluated
evaluateFor graph cur (ExDesc tptr con) = case con of
    IR name =>
    ER name =>
    CF mapping =>
    CP connect =>
evaluateFor graph cur (ImDesc ptr op) = case op of
    Apply => apply internal
    Extract => extract internal
    PiT => ImEval internal op
    SigmaT => ImEval internal op
    where internal = evaluateFor graph (graph ! ptr)

apply :: Evaluated -> Maybe Evaluated
apply pair = case ev of
    ExEval context => case context of
        EF mapping => case mapping of
            EL id expr => Just $ substitute id expr dep
            EB list => case dep of
                ExEval shouldBeName => case shouldBeName of
                    ExtEv maybeName => case maybeName of
                        ExtName name => Just $ snd (find (\x -> fst x == name) list)
                        _ => Nothing
                    _ => Nothing
                ImEval _ _ => Nothing
        DepRef _ => Just $ ImEval pair Apply
        _ => Nothing
    ImEval intern op => case op of
        Apply => Just $ ImEval pair Apply
        Extract => Just $ ImEval pair Apply
        PiT => Nothing
        SigmaT => Nothing
    where (ev, dep) = disassemblePair

extract :: Evaluated -> Maybe Evaluated
extract pair = ifPresent snd $ disassemblePair pair


disassemblePair :: Evaluated -> Maybe EvalConnect
disassemblePair (ExEval context) => case context of
    EP connect => Just connect
    _ => Nothing


substitute :: ID -> Evaluated -> Evaluated -> Evaluated
substitute id dep expr = case expr of
    ExEval context => case context of
        DepRef rid => if id == rid then dep else expr
        ExtEv _ => expr
        EF mapping => case mapping of 
            EL otherId evaluated => EL otherId $ subsSame evaluated
            EB list => EB $ map (\v -> (fst v, subsSame $ snd v)) list
        EP connect => case connect of
            EC func depen => EC (subsSame func) (subsSame depen)
    ImEval inter op => evalutateOp $ ImEval (subSame inter) op
    where subsSame = substitute id dep
