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
        props :: func {
            // Represents Properties
            length :: Int;
            headPtr :: Ptr F;
        } -> {
            ElGetter := (FromInt T) (index :: Int) -> OffGet(props.headPtr, index).value;
            IterFor := (pIndex :: Int) -> (index : pIndex);
            IterImpl := Image(IterFor);
        } ~ AsIterable T IterImpl {
            // Inheritance Definition
            head : IterFor 0,
            next : ( ite :: IteImpl -> {
                ind := ite.index + 1
            } ~ Choose(Optional(IteImpl)) (ind < props.length) ( AsOpt(IterFor(ind))), NullOpt(IterImpl) )
        } | {
            // Exposed Methods
            length : props.length,
        } | ElGetter;
    );

    // Array type definition
    Array := T -> Image(ArrayWith T);

    // Set function
    Set := F -> param :: func { array :: Array F; index :: Int; value :: F; } -> {
        newPtr := OffSet(param.array.head.pointer, param.index, param.value);
    } ~ ArrayWith(param.array.length, newPtr);


    // Creates an array
    NewArray := F -> (size :: Int) -> Comp(
        WrapS NewPtr F,
        WrapT(pointer :: Ptr F -> ArrayWith(size, pointer))
    );

    // Deletes an array
    DelArray := F -> Comp(
        WrapT(array :: Array F -> array.head.pointer),
        WrapC DelPtr F
    );

    AsArray := F -> (
        (a :: F) -> Comp(
            NewArray F 1,
            WrapT(array :: Array F -> Set(array, 0, a))
        ) |
        // Double parameter
        (a1 :: F, a2 :: F) -> Comp(
            NewArray F 2,
            WrapT Comp(
                array :: Array F -> Set(array, 0, a1),
                array :: Array F -> Set(array, 1, a2)
            )
        ) |
        // Triple parameter
        (a1 :: F, a2 :: F, a3 :: F) -> Comp(
            NewArray F 3,
            WrapT Comp(
                array :: Array F -> Set(array, 0, a1),
                array :: Array F -> Set(array, 1, a2),
                array :: Array F -> Set(array, 2, a3)
            )
        ),
    );
}