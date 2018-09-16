import "common/format/Import"
import -> export {
    import None := import {"common/io/InOut", "common/system/System"}

    Console -= {System} |-> OutStream;
}