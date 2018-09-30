"Generics" in "common.generics"
include "common/format/Import"

{
    /*
     * @arg set of input and output
     * @ret { Set of Transforms from the specified input to the specified output. }
     */
    Transform := (I, O) -> (I ~> O);

    /*
     * @arg certain set
     * @ret { Set of transformation on the certain type }
     */
    Self := T -> Transform(T, T);

    // Identity
    Id := T -> (Self(T)) (v :: T -> v);

    // Consumer and Supplier
    Consumer := V -> I -> ( func { mass :: V; input :: I; } ~> V );
    Supplier := V -> O -> ( V ~> func { mass :: V; output :: O; } );

}