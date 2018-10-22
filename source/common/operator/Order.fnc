"Order" in "common.order"

import ["common.generics.Generics", "common.generics.Commons"] ~ {
    Ordering = \T : (
        \param = {
            lessThan = Function (T:T);
            moreThan = Function (T:T);
            lessEqThan = Function (T:T);
            moreEqThan = Function (T:T);
        } : param;
    );
}