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

type ClauseGraph = Map.Map Address Clause


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
typeCheck :: (ClauseGraph, Links) -> Address -> Mismatches
typeCheck (graph, lk) addr = typeCheckFor (graph, lk) addr (graph ! addr)

typeCheckFor :: (ClauseGraph, Links) -> Address -> ClauseDesc -> Mismatches
typeCheckIn (graph, lk) addr (ExDesc td cd) = 


-- Computation (For now, preserve types purely for display)
data Extern = Empty | ExtName Name
data VOp = VI | VE Extern | VAp | VEx | VPT | VST | VCF MapOp | VCP
type EvalClause = [(Address, VOp, SubNode)]

compute :: EvalClause -> EvalClause

computeFor :: EvalClause -> Address -> VOp -> Maybe EvalClause
computeFor whole addr VI = Nothing   -- This is the hardest part
computeFor whole addr VE ext = Just ext -- Computed part
computeFor whole addr VAp = applied where
    apply :: EvalClause -> EvalClause
    apply 
computeFor whole addr VEx = extracted where
    extract :: EvalClause -> EvalClause
computeFor whole addr VPT = pitype
computeFor whole addr VST = sigmatype
computeFor whole addr (VCF op) = f
computeFor whole addr VCP = f

