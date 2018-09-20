"List" in "common.collection"
include "lang.format.Import"

import {"lang.generics.Generics", "lang.collection.Ites"} ~ {
    List := T -> (Iterable(T)) (
        // Iterator implementation, which is a subset of Ite
        IteImpl : {
            ListNode -= @Ite(T);
            ListNode := (T value, Optional(ListNode) next);
        } ~ ListNode,

        // The head node
        IteImpl head,

        // next Implementation
        next : (IteImpl impl -> impl($next)($value))
    );
}