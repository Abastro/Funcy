"Range" in "common.integer"

import {"common.control.Controls"} ~ {
    //TotalOrdered = ;

    hid RangeInput. {
        \pair = (Int ? : Int ?). pair;
        \pair = (Int ? : [Int ?]). pair;
        \pair = ([Int ?] : Int ?). pair;
        \pair = ([Int ?] : [Int ?]). pair;
    };

    InRange. \pair = RangeInput ?. ToBool Int ?;
    InRange. (
        \pair = (Int ? : Int ?) = ToBool Int (\i = Int ? : ((InOf pair < i) * (i < OutOf pair)) i) |
        \pair = (Int ? : [Int ?]) = ToBool Int (\i = Int ? : ((InOf pair < i) * (i <= OutOf pair)) i) |
        \pair = ([Int ?] : Int ?) = ToBool Int (\i = Int ? : ((InOf pair <= i) * (i < OutOf pair)) i) |
        \pair = ([Int ?] : [Int ?]) = ToBool Int (\i = Int ? : ((InOf pairin <= i) * (i <= OutOf pair)) i)
    );

    Range = \pair = RangeInput ? : Assert Int (InRange pair);
}