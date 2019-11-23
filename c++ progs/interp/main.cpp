#include <vector>
#include <string>
#include <iostream>
#include <stdexcept>
#include <fstream>

using namespace std;

ifstream in("input.txt");
ofstream out("output.txt");

const int VAL(1);
const int VAR(2);
const int ADD(3);
const int IF(4);
const int LET(5);
const int FUNCTION(6);
const int CALL(7);

class expression;

expression* error(){
    out << "ERROR";
    return nullptr;
}

class env{
    vector<pair<string, expression*>> token;
public:
    env() = default;
    env(env& that){
        token = that.token;
    }
    void getNextToken(const string& i, expression* v){
        pair<string, expression*> temp;
        temp.first = i;
        temp.second = v;
        token.push_back(temp);
    }
    expression* fromEnv(string& id){
        for (auto & i : token)
            if (id == i.first) return i.second;
        return nullptr;
    }
    ~env() = default;
};

class expression{
public:
    virtual int getV() {
        throw invalid_argument("getValue can be called only from val");
    };
    virtual bool isFunction() {
        return false;
    }
    virtual expression* eval() = 0;
    virtual void output(){
        out << "ERROR";
    }
    virtual ~expression()= default;;
};
class val:public expression{
    int value;
public:
    explicit val(int v): value(v){}
    int getV () final{
        return value;
    }
    expression* eval(){
        return new val(value);
    }
    void output(){
        out << "(val " << value << ")";
    }
    ~val() = default;
};
class var:public expression{
    string id;
    env& domain;
public:
    explicit var(const string& i, env& d): id(i), domain(d) {}
    expression* eval(){
        return domain.fromEnv(id);
    }
    ~var() = default;
};

template <class T>
int getValue(T n){
    throw invalid_argument("getValue can be called only from val");
}

int getValue(expression* n){
    return n->getV();
}

class add:public expression{
    expression* first;
    expression* second;
public:
    add(expression* f, expression* s): first(f), second(s){}

    expression* eval(){
        expression* f = first->eval();
        expression* s = second->eval();
        expression* result;
        if (f && s) {
            result = new val(getValue(f) + getValue(s));
        } else result = nullptr;
        return result;
    }

    ~add(){
        delete first;
        delete second;
    };
};
class If:public expression{
    expression* e1;
    expression* e2;
    expression* e_then;
    expression* e_else;
public:
    If(expression* E1, expression* E2, expression* Then, expression* Else): e1(E1), e2(E2), e_then(Then), e_else(Else){}

    expression* eval(){
        auto E1 = e1->eval();
        auto E2 = e2->eval();
        if (E1 && E2)
            return getValue(E1) > getValue(E2) ? e_then->eval() : e_else->eval();
        return nullptr;
    }

    ~If(){
        delete e1;
        delete e2;
        delete e_then;
        delete e_else;
    }
};
class let:public expression{
    //string id;
    //expression* value;
    expression* body;
public:
    let(const string& i, expression* value, expression *b, env& d){
        d.getNextToken(i, value->eval());
        body = b;
    }
    expression* eval(){
        return body->eval();
    }
    ~let(){
        delete body;
    }
};
class function:public expression{
    string id;
    expression* body;
public:
    function(const string& i, expression* b): id(i), body(b){}
    bool isFunction(){
        return true;
    }
    expression* eval(){
        return new function(id, body);
    }
    pair<string, expression*> get_info(){
        pair<string, expression*> result;
        result.first = id;
        result.second = body;
        return result;
    }
    ~function(){
        if (body) delete body;
    }
};
class call:public expression{
    expression* f_expr;
    expression* arg_expr;
    env& domain;
public:
    call(expression* a, expression* b, env& d): f_expr(a), arg_expr(b), domain(d){}

    expression* eval(){
        expression* temp = f_expr->eval();
        if (temp->isFunction()) {
            auto result = dynamic_cast<function *>(temp);
            domain.getNextToken(result->get_info().first, arg_expr->eval());
            return result->get_info().second->eval();
        } else return nullptr;
    }
    ~call(){
        delete f_expr;
        delete arg_expr;
    }
};

string moveCursor(char c){
    string temp;
    getline(in, temp, c);
    in.unget();
    return temp;
}

class interpretator{
    env domain;
    expression* body;
    expression* ready;
    int recognize(string &type){
        if (type == "val") return VAL;
        if (type == "var") return VAR;
        if (type == "add") return ADD;
        if (type == "if") return IF;
        if (type == "let") return LET;
        if (type == "function") return FUNCTION;
        if (type == "call") return CALL;
        return 0;
    }
public:
    interpretator() {
        body = read_expr();
        ready = nullptr;
    }
    expression* read_expr(){
        expression* result = nullptr;
        if (in.get() == '('){
            string s;
            in >> s;
            switch (recognize(s)) {
                case VAL: {
                    int v;
                    in >> v;
                    result =  new val(v);
                } break;
                case VAR: {
                    string i;
                    in.get();
                    getline(in, i, ')');
                    in.unget();
                    result =  new var(i, domain);
                } break;
                case ADD: {
                    moveCursor('(');
                    expression *first = read_expr();
                    if (first) {
                        moveCursor('(');
                        expression *second = read_expr();
                        if (second) result = new add(first, second);
                    }
                } break;
                case IF: {
                    moveCursor('(');
                    auto e1 = read_expr();
                    moveCursor('(');
                    auto e2 = read_expr();
                    if (moveCursor('(').find("then")) {
                        auto e_then = read_expr();
                        if (moveCursor('(').find("else")) {
                            auto e_else = read_expr();
                            result = new If(e1, e2, e_then, e_else);
                        }
                    }
                } break;
                case LET: {
                    string i;
                    expression* v;
                    expression* b;
                    in >> i;
                    string temp;
                    getline(in, temp, '(');
                    in.unget();
                    if (temp.find('=')) {
                        v = read_expr();
                        getline(in, temp, '(');
                        in.unget();
                        if (temp.find("in")) {
                            b = read_expr();
                            result = new let(i, v, b, domain);
                        }
                    }
                } break;
                case FUNCTION: {
                    string i;
                    in >> i;
                    moveCursor('(');
                    result = new function(i, read_expr());
                } break;
                case CALL: {
                    moveCursor('(');
                    auto f_expr = read_expr();
                    moveCursor('(');
                    auto arg_expr = read_expr();
                    result = new call(f_expr, arg_expr, domain);
                } break;
            }
            if (in.get() == ')' && result) {
                return result;
            } else {
                return nullptr;
            }
        } else return nullptr;
    }
    void execution(){
        if (body) {
            ready = body->eval();
            if (ready) ready->output(); else out << "ERROR";
        } else out << "ERROR";
    }
    ~interpretator(){
        delete body;
        delete ready;
    }
};

int main(){
    if (in.is_open() && out.is_open()) {
        interpretator S;
        S.execution();
    }
    in.close();
    out.close();
    return 0;
}