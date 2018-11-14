data Name = Name String

data Qualifier = Qual Name Clause

data Block = AsBlock [(Name, Clause)]

data Lambda = AsLambda Qualifier Clause

data Clause = CR Name | CL Lambda | CB Block | CApp Clause Clause

evaluate :: Clause -> Evaluated
evaluate (CR name) = Hole name
evaluate (CL (AsLambda (Qual param type) result)) = lambda param ( checkType (evaluate type) ) (evaluate result)
evaluate (CB (AsBlock ((name, clause) : remaining) ) ) = evaluate(AsBlock remaining)
evaluate (CApp fn var) = apply ( checkFunc (evaluate fn) ) (evaluate var)


instance Show Name where
    show (Name name) = "$" ++ name

getData :: Name -> String
getData (Name name) = name

contents :: Block -> String
contents ( AsBlock [] ) = ""
contents ( AsBlock ((name, block) : remaining) ) = " " ++ getData name ++ "=" ++ show block ++ "," ++ contents (AsBlock remaining)

instance Show Block where
    show block = "{" ++ contents block ++ "}"