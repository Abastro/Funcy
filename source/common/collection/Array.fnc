"Array" in "common.collection"
include "lang.format.Import"

import {
    "common.generics.Generics",
    "common.generics.Utilities",
    "common.control.Constrols",
    "common.memory.Memory",
    "common.collection.Ites"
} ~ {
    hid ArrayWith := T -> (
        (
            // Represents Fields
            pLength :: Int,
            headPtr :: Ptr(F)
        ) -> {
            ElGetter := (FromInt(T)) (index :: Int) -> OffGet(headPtr, index).value;
            IterFor := (pIndex :: Int) -> AsContainer(ElGetter(pIndex)) | (index : pIndex);
            IterImpl := Image(IterFor);
        } ~ AsIterable(T)(IterImpl) {
            // Inheritance Definition
            pHead : IterFor(0),
            pNext : ( ite -= IteImpl -> {
                ind := ite.index + 1
            } ~ Choose(Optional(IteImpl)) (ind < pLength) ( AsOpt(IterFor(ind))), NullOpt(IterImpl) )
        } | {
            // Exposed Methods
            length : pLength,
        } | ElGetter;
    );

    // Array type definition
    Array := T -> Image(ArrayWith(T));

    // Set function
    Set := F -> (array :: Array(F), index :: Int, value :: F) -> {
        newPtr := OffSet(array.head.pointer, index, value);
    } ~ ArrayWith(array.length, newPtr);


    // Creates an array
    NewArray := F -> size :: Int -> Comp(
        WrapS(NewPtr(F)),
        WrapT(Ptr(F) pointer -> ArrayWith(size, pointer))
    );

    // Deletes an array
    DelArray := F -> Comp(
        WrapT(Array(F) array -> array.head.pointer),
        WrapC(DelPtr(F))
    );

    AsArray := F -> (
        // Single parameter
        (a1 :: F) -> Comp(
            NewArray(F)(1),
            WrapT(Array(F) array -> Set(array, 0, a1))
        ),

        // Double parameter
        (a1 :: F, a2 :: F) -> Comp(
            NewArray(F)(2),
            WrapT(Comp(
                Array(F) array -> Set(array, 0, a1),
                Array(F) array -> Set(array, 1, a2)
            ))
        ),

        // Triple parameter
        (a1 :: F, a2 :: F, a3 :: F) -> Comp(
            NewArray(F)(3),
            WrapT(Comp(
                Array(F) array -> Set(array, 0, a1),
                Array(F) array -> Set(array, 1, a2),
                Array(F) array -> Set(array, 2, a3)
            ))
        ),
    );
}