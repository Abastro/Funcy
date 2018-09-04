// Tutorials - Surely, a bad one, which means you won't get it if you're five
// Ah, right. You need to know programming basics for this.
import lib(#common.basis.Import)
(import inc) -> export {
    import None := #common.basis.Format

    // This is a common syntax to declare a 'funcy'. It describes, basically, every object in this language.
    // Here, you can see a funcy is declared with the name on the left('TF') to have value on the right.
    // `:=` is the declaration operator, which declares the funcy in the right with the name `TF` on the left.
    // `Bool` is a funcy which serves as a type for booleans.
    // `Bool switch` part declares switch as a boolean parameter, which can be used on the latter expression.
    TF := Bool switch -> !switch

    // Substitutes the parameter with actual boolean value, which evaluates the funcy as well.
    // Instructions unclear? Then just don't post issues on my github pls
    ThisIsFalse := TF(TRUE)

    // `Int` is a funcy for integers - now first expression `Int i -> i+1` part *should* make sense.
    // `thisIsZero` is the field name - it doesn't have the type declared, if you look at it carefully.
    // So second expression `thisIsZero -> TF(FALSE)` maps field $thisIsZero to TF(FALSE) which is 0.
    // The compound `(Expr1, Expr2)` means it can map both of the inputs of Expr1 and Expr2 to their outputs.
    // This means they should both have disjoint input. (Usually it means differing type)
    TF2 := (Int i -> i + 1, thisIsZero : TF(FALSE))

    // This is what I meant. Don't ask why these are named like that.
    ThisIsTwo := TF2(1)
    ThisIsZero := TF2($thisIsZero)

    // The number of expression is not limited to two.
    TF3 := (Int i -> i * 2, what : 3,  "Hello" : "World")
    ThisIsThree := TF3($what)
    ThisIsFour := TF3(2)
    ThisIsWorld := TF3("Hello") // "World"
    // ThisIsError := TF3("Welp") gives error because it does not have any mapping

    // Now take a look at mundane declaration! Is this typedef?
    TF3 := TF2

    // From now on, virtual funcies would be discussed.
    // Here, the parameter name is omitted. This imply it takes integer parameters, which gives boolean output.
    // Of course, TF4(0) will give error as it does not have such mapping specified.
    // Wait, you don't get why it's undefined? Screw that, kiddos. Stop reading this.
    TF4 := Int -> %Bool

    // inherits declares the inheritance. Here it means TF5 can be used in the place of TF4 in substitution.
    // It is applied to the last expression which is grouped.
    TF5 := (Int i -> i != 0) inherits TF4

    // You can look at the usage here. Note that every funcy could be a type.
    // Anything which inherits funcy TF4 could have the type TF4.
    // Here, val(0) could exist as val is not determined.
    // Here you can see the purpose of virtual function - which exists to encapsulate things in opaque definitions.
    TF6 := TF4 val -> val(0)

    ThisISFalse := TF6(TF5)

    // This means field $value will give integers.
    // TF4($value) will give error, as it's not specified.
    // Instead, it could be used to call inherited funcies with virtual type.
    TF7 := value : Int

    TF8 := (value : 7) inherits TF7

    // Again, V($value) could exist as val is not determined.
    TF9 := TF7 val -> val($value)

    // Lazy evaluation should allow this - the applicability could be determined in compile-time, though.
    ThisIsSeven := TF9(TF8)

    // Now compounds being virtual: It's just compound of virtual funcies.
    // No comments for this, figure it out yourself.
    TF9 := (number : %Int, Int -> %Int)
    TF10 := (number : 10, Int i -> i / 2) inherits TF10

    TF11 := TF9 val -> val($number)
    TF12 := TF9 val -> val(11)

    ThisIsTen := TF11(TF10)
    ThisIsFive := TF12(TF10)

    // When it needs many declarations to evaluate the result, you can use parameter-dependent block in this way.
    // Note that it thinks num is declared in the block, and gives new declarations from it
    TFLong := Int num ~ {
        Int sqr := num * num
       Int result := num + 1
    } -> result

    // Basics are all covered now.
    // Everything from now on is just shortcuts.
    // TODO - describe shortcuts, especially with compounds - because they are annoying to write as whole.
}