import "common/format/Import"
import -> export {
    import None := import "common/format/Format";

    /*
     * @arg set of input and output
     * @ret { Set of Transforms from the specified input to the specified output. }
     */
    Transform := (I, O) -> (I |-> O);

    /*
     * @arg certain set
     * @ret { Set of transformation on the certain type }
     */
    Self := T -> Transform(T, T);

    // Identity
    Id := T -> (Self(T))(T v -> v);

    // Pair
    Pair := (T, S) -> (T left, S right);

    // Consumer and Supplier
    Consumer := (V) -> (I) -> ( (V mass, I input) |-> V );
    Supplier := (V) -> (O) -> ( V |-> (V mass, O output) );

}