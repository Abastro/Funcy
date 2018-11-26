-- Basis, Block / Lambda / Dependent Type

type Name = String

data Lambda = AsLambda Name Clause
data Block = AsBlock [(Name, Clause)]
data Mapping = ML Lambda | MB Block

type Type = Clause
type Pair = Clause
type Function = Clause

data Clause = IntRef Name | ExtRef Name |
    Apply Pair | Extract Pair | -- extract dependency part of the pair
    PiT Function | SigmaT Function |
    CFunc (Maybe Type) Mapping | CPair (Maybe Type) Function Clause
 
-- Convert a clause into Address tree
data MapOp = Lm | Bl
data Operation = IR Name | ER Name | Ap | Ex | PT | ST | CF MapOp | CP
type Address = String
type SubNode = [(String, Address)]
type DistClause = [(Address, Operation, SubNode)]

analyzeF :: Clause -> DistClause
analyzeF cl = analyze "" cl

analyze :: Address -> Clause -> DistClause

analyze base (IntRef ref)       = [(base, IR ref, [])]
analyze base (ExtRef ref)       = [(base, ER ref, [])]

analyze base (Apply pair)       = [(base, Ap, [("", pairAddr)])] ++ analyze pairAddr pair where pairAddr = base ++ "#"
analyze base (Extract pair)     = [(base, Ex, [("", pairAddr)])] ++ analyze pairAddr pair where pairAddr = base ++ "%"

analyze base (PiT func)         = [(base, PT, [("", funcAddr)])] ++ analyze funcAddr func where funcAddr = base ++ "&"
analyze base (SigmaT func)      = [(base, ST, [("", funcAddr)])] ++ analyze funcAddr func where funcAddr = base ++ "|"

analyze base (CFunc tp mp)      = [(base, CF (mpMode mp), opSub "$" base tp ++ mpSub base mp)] ++ optAnalyze "$" base tp ++ mpAnalyze base mp
analyze base (CPair tp func dep)= [(base, CP, opSub "$" base tp ++ [("@", base ++ "@"), ("*", base ++ "*")])] ++ optAnalyze "$" base tp ++ analyze (base ++ "@") func ++ analyze (base ++ "*") dep

opSub :: String -> Address -> Maybe Type -> SubNode
opSub tag base (Just tp) = [(tag, base ++ tag)]
opSub tag base Nothing = []

optAnalyze :: String -> Address -> Maybe Type -> DistClause
optAnalyze tag base (Just tp) = analyze (base ++ tag) tp
optAnalyze tag base Nothing = []

mpMode :: Mapping -> MapOp
mpMode ML x = Lm
mpMode MB x = Bl

mpSub :: Address -> Mapping -> SubNode
mpSub base (ML (AsLambda inp outp)) = [(inp, base ++ inp)]
mpSub base (MB (AsBlock list)) = map (\x -> (fst x, base ++ fst x)) list

mpAnalyze :: Address -> Mapping -> DistClause
mpAnalyze base (ML (AsLambda inp outp)) = analyze (base ++ inp) outp
mpAnalyze base (MB (AsBlock list)) = concat ( map (\x -> analyze (base ++ fst x) snd x) list )


-- Traverse the tree, fetching long-range link information
type Refs = [(Name, Address)]
type RefLinks = [(Address, Address)]
data Links = AsLink Refs RefLinks

findLink :: DistClause -> Address -> Links
findLink dc addr = findLinkIn dc (fetch dc addr)

findLinkIn :: DistClause -> (Address, Operation, SubNode) -> Links
findLinkIn dc (addr, op, subs) = mergeTwo found (findLinkFor dc addr found op) where found = mergeList ( map (\x -> findLink dc (snd x)) subs )

findLinkFor :: DistClause -> Address -> Links -> Operation -> Links
findLinkFor dc addr found (IR name) = AsLink [(name, addr)] []
findLinkFor dc addr found (ER name) = AsLink [] []
findLinkFor dc addr found Ap = AsLink [] []
findLinkFor dc addr found Ex = AsLink [] []
findLinkFor dc addr found PT = AsLink [] []
findLinkFor dc addr found ST = AsLink [] []
findLinkFor dc addr (AsLink refs links) (CF mop) = 
findLinkFor dc addr found CP = AsLink [] []

-- Typecheck

-- Erase types