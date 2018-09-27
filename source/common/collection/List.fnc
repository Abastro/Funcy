"List" in "common.collection"
include "lang.format.Import"

import {"lang.generics.Generics", "lang.collection.Ites"} ~ {
    /*
     * @arg the type set of List
     * @ret { Set of Lists. }
     */
    List := T -> {
        Con := Container(T);
        ListImpl :: ExtSet(Con);
        ListImpl := (
            (value -= T) -> AsIterable(T)(Con)(pHead : AsContainer(T)(value), next : value -= Con -> NullOpt(Con)),
            (value -= T, list -= ListImpl) -> AsIterable(T)(ListImpl)(
                pHead : list,
                pNext : 
            )
        );
    } -> ListImpl;
}