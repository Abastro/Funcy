#include <vector>
#include <set>
#include <map>
#include <string>

// TODO Pre-evaluating certain routines
// Declared funcies will have its value stored, while anonymous funcies will just have the temporal value.

class FuncyFrame;

class Funcy {
public:
    Funcy(FuncyFrame* frame) : frame(frame), blueprint(NULL) { }
    Funcy(FuncyFrame* frame, Funcy* blueprint) : frame(frame), blueprint(blueprint) {}

    virtual bool match(Funcy*);
    virtual Funcy* evaluate(Funcy*);
private:
    const FuncyFrame* const frame;
    const Funcy* const blueprint; // For anonymous funcies which doesn't get any references
};

class FuncyFrame {
    bool inherits(Funcy* funcy) { return inheritedTypes.find(*funcy) != inheritedTypes.end(); }
private:
    std::map<Funcy, Funcy> ios;
    std::set<Funcy> inheritedTypes;
};



class NativeFuncy : Funcy {
public:
    bool match(Funcy& funcy) { return true; }
    Funcy& evaluate(Funcy& funcy) { return *this; }
};

class ReferenceFuncy : Funcy {
public:
    bool match(Funcy& funcy) { return true; }
private:
    std::string name;
};

class ContainmentFuncy : Funcy {
public:
    ContainmentFuncy(ReferenceFuncy* refName, Funcy* value) : Funcy(NULL), refName(refName), value(value) { }

    bool match(Funcy* funcy) { return funcy == (Funcy*)this->refName; }
    Funcy* evaluate(Funcy* funcy) { return this->value; }
private:
    ReferenceFuncy *const refName;
    Funcy *const value;
};

// A class which won't be used frequently - What's needed will be pre-selected.
class CompoundFuncy : Funcy {
public:
    bool match(Funcy* funcy) {
        for(int i = 0; i < internals.size(); i++)
            if(internals[i].match(funcy))
                return true;
        return false;
    }

    Funcy* evaluate(Funcy* funcy) {
        for(int i = 0; i < internals.size(); i++)
            if(internals[i].match(funcy))
                return internals[i].evaluate(funcy);
        throw 1;
    }
private:
    std::vector<Funcy> internals;
};
