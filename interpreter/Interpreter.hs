data Reference = Ref String
data Block = AsBlock [(Reference, Block)]





instance Show Reference where
    show (Ref name) = "$" ++ name

getData :: Reference -> String
getData (Ref name) = name

contents :: Block -> String
contents ( AsBlock [] ) = ""
contents ( AsBlock ((ref, block) : remaining) ) = " " ++ getData ref ++ "=" ++ show block ++ "," ++ contents (AsBlock remaining)

instance Show Block where
    show block = "{" ++ contents block ++ "}"