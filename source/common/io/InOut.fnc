"InOut" in "common.io"
include "lang.format.Import"

import {"lang.generics.Generics", "lang.generics.Utilities"} ~ {
    // Out-related interfaces
    AsWriterType : OutState -> Consumer OutState String;
    Writer : AsWriterType ?;

    AsOutStreamType : OutState -> State (AsWriterType OutState) OutState
    OutStream : AsOutStreamType ?;

    Print := T -> (toString : ToString T) -> (Consumer OutStream:T) (
        param :: func {stream : OutStream, toPrint : T} -> (
            // Transition of the stream
            StateT(
                trans : func {state : OutState, writer : Writer} -> trans.writer(trans.state, toString param.toPrint)
            ) param.stream
        )
    )
}
