#include <vector>
#include <set>
#include <map>
#include <string>

// TODO Pre-evaluating certain routines
// Declared funcies will have its value stored, while anonymous funcies will just have the temporal value.

class Funcy;
class FuncyFrame;
template<typename N>
class NativeTypeFuncy;
template<typename N>
class NativeValueFuncy;

// Interpretation part



// Running part

class Funcy {
public:
    Funcy(FuncyFrame* frame) : frame(frame) { }

    // Evaluation of the new Funcy
    virtual Funcy* evaluate(Funcy*);

    FuncyFrame* getFrame() { return this->frame; }
private:
    FuncyFrame* const frame;
};

class FuncyFrame {
public:
    FuncyFrame(std::set<Funcy> inheritedTypes) : inheritedTypes(inheritedTypes) {}

    // Construct the new funcy with given parameters
    virtual Funcy* construct(std::vector<Funcy*>& params);

    // Find matching input type
    virtual Funcy* matchingInputType(Funcy*);

    // Find output type - maybe not needed
    virtual Funcy* outputType(Funcy*);

    // Check for inheritance
    bool inherits(Funcy* funcy) { return inheritedTypes.find(*funcy) != inheritedTypes.end(); }
private:
    const std::set<Funcy> inheritedTypes;
};

// Natives
template<typename N>
class NativeTypeFuncy : Funcy {
public:
    Funcy& evaluate(Funcy& funcy) { throw 1; }
};

template<typename N>
class NativeValueFuncy : Funcy {
public:
    NativeFuncy(N value) : value(value) { }

    Funcy* evaluate(Funcy* funcy) { return this; }
private:
    const N value;
};

// Lambda
class LambdaFuncy : Funcy {
public:
    virtual Funcy* evaluate(Funcy* funcy);
private:
};

// Reference
class ReferenceFuncy : Funcy {
public:
private:
    std::string name;
};

// Containment
class ContainmentFuncy : Funcy {
public:
    ContainmentFuncy(ReferenceFuncy* refName, Funcy* value) : Funcy((FuncyFrame*)NULL), refName(refName), value(value) { }

    Funcy* evaluate(Funcy* funcy) { return this->value; }
private:
    ReferenceFuncy *const refName;
    Funcy *const value;
};

// Compound
// A class which won't be used frequently - What's needed will be pre-selected.
class CompoundFuncy : Funcy {
public:
    Funcy* evaluate(Funcy* funcy) {
        for(int i = 0; i < internals.size(); i++) {
            Funcy& func = internals[i];
            Funcy* inputType = func.getFrame()->matchingInputType(funcy);
            if(inputType != NULL)
                return func.evaluate(funcy);
        }
        throw 1;
    }
private:
    std::vector<Funcy> internals;
};
