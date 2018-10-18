"Utilities" in "common.generics"
include "lang.format.Import"

import {"commons.generics.Generics"} ~ {
    // Optionals
    hid RawOpt : T -> (
        (theValue : T ?) -> {
            getOrDef : (def : T ?) -> theValue;
            isPresent : TRUE;
        } | {} : {
            getOrDef : (def : T ?) -> def;
            isPresent : FALSE;
        }
    );

    Optional : T -> opt : RawOpt T ? -> {
        getOrDef : Self T opt.getOrDef;
        isPresent : Bool opt.isPresent;
    }

    AsOpt : T -> value : T ? -> Optional T (RawOpt T value);
    NullOpt = T -> Optional T (RawOpt {});

    // States
    State : C -> S -> (
        prop : TypeP (C:S) ? -> { const : InP prop; var : OutP prop; }
    );

    // State Transition
    StateT : C -> S -> consumer : Consumer S C ? -> Self (State C S) (
        state : State C S ? -> State C S (
            state.const : consumer(state.var : state.const)
        )
    );

    // Wraps
    Wrap : V -> C -> (
        prop : TypeP (V:C) ? -> { value : InP prop; content : OutP prop; }
    );

    // Wrap 
    WrapC := V -> I -> consumer :: Consumer V I ? -> (
        wrapped : Wrap V I ? -> Wrap V Void (
            consumer (wrapped.value : wrapped.content) : {}
        )
    );

    // Wrap Expansion
    WrapS : V -> O -> supplier : Supplier V O ? -> (
        wrapped : Wrap V ? ? -> Wrap V O (supplier wrapped.value)
    );

    // Wrap Transformation
    WrapT : V -> I -> O -> transform : Transform I O ? -> (
        wrapped : Wrap V I ? -> Wrap V O (wrapped.value : transform wrapped.content)
    );
}