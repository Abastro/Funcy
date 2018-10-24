"Utilities" in "common.generics"
include "lang.format.Import"

import {"commons.generics.Generics"} ~ {
    // Optionals
    Optional : \T : \theValue = NullOr T ? : {
        getOrDef : Self T ?;
        isPresent : Bool ?;
    };

    Optional : \T : {
        \theValue = T ? : {
            getOrDef : Self T (\def = T ? : theValue);
            isPresent : Bool TRUE;
        };
        {} : {
            getOrDef : Self T (\def = T ? : def);
            isPresent : Bool FALSE;
        };
    };

    AsOpt : \T : \value = T ? : Optional T value;
    NullOpt : \T : Optional T {};

    // States
    State : \C : \S : (
        \prop = TypeP (C:S) ? : { const : InP prop; var : OutP prop; }
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