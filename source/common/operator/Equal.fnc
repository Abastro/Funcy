"Order" in "common.order"

import ["common.generics.Generics", "common.generics.Commons"] ~ {
    Equivalence = \T : (
        \equiv = ToBool (T:T) :
        Assert (\x=? : \y=? : -equiv(x:y) | equiv(y:x)) Assert (\x=? : \y=? : \z=? : -equiv(x:y) | -equiv(y:z)) | equiv(x:z)) equiv;
    );
}