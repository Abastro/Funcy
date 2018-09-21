"List" in "common.collection"
include "lang.format.Import"

import {"lang.generics.Generics", "lang.collection.Ites"} ~ {
    /*
     * @arg the type set of List
     * @ret { Set of Lists. }
     */
    List := T -> {
        // Iterator implementation, which is a subset of Ite
        IteImpl -= @Ite(T);
        IteImpl := (T value, Optional(IteImpl) next);
    } ~ (Iterable(T)) (
        // Iterator implementation
        Impl : IteImpl,

        // The head node
        IteImpl head,

        // next Implementation
        next : (IteImpl impl -> impl($next))
    );
}