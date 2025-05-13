import sys
import math
import re
import io
import operator as op


class Symbol(str):          # A Scheme Symbol is implemented as a Python str
    pass

def Sym(s, symbol_table={}):
    "Find or create a unique Symbol entry for given str s in symbol table."
    if s not in symbol_table:
        symbol_table[s] = Symbol(s)
    return symbol_table[s]

_quote, _if, _set, _define, _lambda, _begin, _definemacro, = map(Sym,
"quote  if  set!  define  lambda  begin  define-macro".split())

_quasiquote, _unquote, _unquotesplicing = map(Sym, 
"quasiquote  unquote  unquotesplicing".split())


Number = (int, float)       # A Scheme Number is implemented as a Python int or float
Atom = (Symbol, Number)     # A Scheme Atom is a Symbol or Number
List = list                 # A Scheme List is implemented as a Python list
Exp = (Atom, List)          # A Scheme expression is an Atom or List
Env = dict                  # A Scheme environment (defined below) is a mapping of {variable: value}


class Env(dict):
    "An environment: a dict of {'var':val} pairs, with an outer Env."
    def __init__(self, parms=(), args=(), outer=None):
        # Bind parm list to corresponding args, or single parm to list of args
        self.outer = outer
        if isinstance(parms, Symbol): 
            self.update({parms:list(args)})
        else:
            # Added &rest parameters
            if '&rest' in parms:
                rest_idx = parms.index('&rest')
                args = list(args)[:rest_idx] + [[]] + [list(args)[rest_idx:]]   # added list() cast for &rest macros

            if len(args) != len(parms):
                raise TypeError('expected %s, given %s, ' 
                                % (to_string(parms), to_string(args)))
            self.update(zip(parms,args))
    def find(self, var):
        "Find the innermost Env where var appears."
        if var in self: return self
        elif self.outer is None: raise LookupError(var)
        else: return self.outer.find(var)


class Procedure(object):
    "A user-defined Scheme procedure."

    def __init__(self, parms, exp, env):
        self.parms, self.exp, self.env = parms, exp, env

    def __call__(self, *args): 
        return eval(self.exp, Env(self.parms, args, self.env))
    

EOF_OBJECT = Symbol('#<EOF-OBJECT>') # Note: uninterned; can't be read

class InPort(object):
    "An input port. Retains a line of chars."
    tokenizer = r'''\s*(,@|[('`,)]|"(?:[\\].|[^\\"])*"|;.*|[^\s('"`,;)]*)(.*)'''
    def __init__(self, file):
        self.file = file
        self.line = ''
    def next_token(self):
        "Return the next token, reading new text into line buffer if needed."
        while True:
            if self.line == '': self.line = self.file.readline()
            if self.line == '': return EOF_OBJECT
            token, self.line = re.match(InPort.tokenizer, self.line).groups()
            if token != '' and not token.startswith(';'):
                return token

def read_char(inport):  # TODO: "Do I need this?"
    "Read the next character from an input port."
    if inport.line != '':
        char, inport.line = inport.line[0], inport.line[1:]
        return char
    else:
        return inport.file.read(1) or EOF_OBJECT

quotes = {"'":_quote, "`":_quasiquote, ",":_unquote, ",@":_unquotesplicing}

def read(inport):
    "Read a scheme expression from an input port."
    def read_ahead(token):
        if '(' == token:
            L = []
            while True:
                token = inport.next_token()
                if token == ')': return L
                else: L.append(read_ahead(token))
        elif ')' == token: raise SyntaxError('unexpected )')
        elif token in quotes: return [quotes[token], read(inport)]
        elif token is EOF_OBJECT: raise SyntaxError("unexpected EOF in list")
        else: return atom(token)
    # body of read:
    token1 = inport.next_token()
    return EOF_OBJECT if token1 is EOF_OBJECT else read_ahead(token1)

def atom(token):
    'Numbers become numbers; #t and #f are booleans; "..." string; otherwise Symbol.'
    if token == '#t': return True
    elif token == '#f': return False
    # elif token[0] == '"': return token[1:-1].decode('string_escape')
    elif token[0] == '"': return bytes(token[1:-1], "utf-8").decode("unicode_escape")
    try: return int(token)
    except ValueError:
        try: return float(token)
        except ValueError:
            try: return complex(token.replace('i', 'j', 1))
            except ValueError:
                return Sym(token)
            
def to_string(x):
    "Converts a Python object back into a Lisp-readable string."
    if x is True: return "#t"
    elif x is False: return "#f"
    elif isinstance(x, Symbol): return x
    elif isinstance(x, str): return '"%s"' % x.encode('string_escape').replace('"',r'\"')
    elif isinstance(x, list): return '('+' '.join(map(to_string, x))+')'
    elif isinstance(x, complex): return str(x).replace('j', 'i')
    else: return str(x)

def load(filename):
    "Eval every expression from a file."
    repl(None, InPort(open(filename)), None)

def repl(prompt='lispy>', inport=InPort(sys.stdin), out=sys.stdout):
    "A prompt-read-eval-print loop."
    sys.stderr.write("Lispy version 2.0\n")
    while True:
        try:
            if prompt: print(prompt, end=' ', flush=True, file=sys.stdout)
            x = parse(inport)
            if x is EOF_OBJECT: return
            val = eval(x)
            if val is not None and out: print(to_string(val), file=out)
        except Exception as e:
            print ('%s: %s' % (type(e).__name__, e))


def is_pair(x): return x != [] and isa(x, list)


def callcc(proc):
    "Call proc with current continuation; escape only"
    ball = RuntimeWarning("Sorry, can't continue this continuation any longer.")
    def throw(retval): ball.retval = retval; raise ball
    try:
        return proc(throw)
    except RuntimeWarning as w:
        if w is ball: return ball.retval
        else: raise w


def add_globals(self):
    "Add some Scheme standard procedures."
    import math, cmath, operator as op
    self.update(vars(math))
    self.update(vars(cmath))
    self.update({
     '+':op.add, '-':op.sub, '*':op.mul, '/':op.truediv, 'not':op.not_, 
     '>':op.gt, '<':op.lt, '>=':op.ge, '<=':op.le, '=':op.eq, 'modulo':op.mod, 'pow':math.pow, 'exp':math.exp,
     'equal?':op.eq, 'eq?':op.is_, 'length':len, 'cons':lambda x,y:[x]+list(y), 'floor':math.floor, 'abs':abs,
     'car':lambda x:x[0], 'cdr':lambda x:x[1:], 'append':lambda x,y:list(y)+[x],  
     'list':lambda *x:list(x), 'list?': lambda x:isa(x,list),
     'null?':lambda x:x==[], 'symbol?':lambda x: isa(x, Symbol),
     'boolean?':lambda x: isa(x, bool), 'pair?':is_pair, 
     'port?': lambda x:isa(x,file), 'apply':lambda proc,l: proc(*l), 
     'eval':lambda x: eval(expand(x)), 'load':lambda fn: load(fn), 'call/cc':callcc,
     'open-input-file':open,'close-input-port':lambda p: p.file.close(), 
     'open-output-file':lambda f:open(f,'w'), 'close-output-port':lambda p: p.close(),
     'eof-object?':lambda x:x is EOFError, 'read-char':read_char,
     'read':read, 'write':lambda x,port=sys.stdout:port.write(to_string(x)),
     'print':lambda x:print(to_string(x)),
     'display':lambda x,port=sys.stdout:port.write(x if isa(x,str) else to_string(x))})
    return self

isa = isinstance

global_env = add_globals(Env())


def eval(x, env=global_env):
    "Evaluate an expression in an environment."
    while True:
        if isinstance(x, Symbol):       # variable reference
            return env.find(x)[x]
        elif not isinstance(x, list):   # constant literal
            return x                
        elif x[0] is _quote:     # (quote exp)
            (_, exp) = x
            return exp
        elif x[0] is _if:        # (if test conseq alt)
            (_, test, conseq, alt) = x
            x = (conseq if eval(test, env) else alt)
        elif x[0] is _set:       # (set! var exp)
            (_, var, exp) = x
            env.find(var)[var] = eval(exp, env)
            return None
        elif x[0] is _define:    # (define var exp)
            (_, var, exp) = x
            env[var] = eval(exp, env)
            return None
        elif x[0] is _lambda:    # (lambda (var*) exp)
            (_, vars, exp) = x
            return Procedure(vars, exp, env)
        elif x[0] is _begin:     # (begin exp+)
            for exp in x[1:-1]:
                eval(exp, env)
            x = x[-1]
        else:                    # (proc exp*)
            exps = [eval(exp, env) for exp in x]
            proc = exps.pop(0)
            if isinstance(proc, Procedure):
                x = proc.exp
                env = Env(proc.parms, exps, proc.env)
            else:
                return proc(*exps)


def parse(inport):
    "Parse a program: read and expand/error-check it."
    # Backwards compatibility: given a str, convert it to an InPort
    if isinstance(inport, str): inport = InPort(io.StringIO(inport))
    return expand(read(inport), toplevel=True)


def expand(x, toplevel=False):
    "Walk tree of x, making optimizations/fixes, and signaling SyntaxError."
    require(x, x!=[])                    # () => Error
    if not isinstance(x, list):                 # constant => unchanged
        return x
    elif x[0] is _quote:                 # (quote exp)
        require(x, len(x)==2)
        return x
    elif x[0] is _if:                    
        if len(x)==3: x = x + [None]     # (if t c) => (if t c None)
        require(x, len(x)==4)
        return list(map(expand, x))            # CHANGED to list()
    elif x[0] is _set:                   
        require(x, len(x)==3); 
        var = x[1]                       # (set! non-var exp) => Error
        require(x, isinstance(var, Symbol), "can set! only a symbol")
        return [_set, var, expand(x[2])]
    elif x[0] is _define or x[0] is _definemacro:
        require(x, len(x)>=3)            
        _def, v, body = x[0], x[1], x[2:]
        if isinstance(v, list) and v:           # (define (f args) body)
            f, args = v[0], v[1:]        #  => (define f (lambda (args) body))
            return expand([_def, f, [_lambda, args]+body], toplevel)    # CHANGED to pass toplevel
        else:
            require(x, len(x)==3)        # (define non-var/list exp) => Error
            require(x, isinstance(v, Symbol), "can define only a symbol")
            exp = expand(x[2], toplevel)    # CHANGED to pass toplevel
            if _def is _definemacro:
                require(x, toplevel, "define-macro only allowed at top level")
                proc = eval(exp)       
                require(x, callable(proc), "macro must be a procedure")
                macro_table[v] = proc    # (define-macro v proc)
                return None              #  => None; add v:proc to macro_table
            return [_define, v, exp]
    elif x[0] is _begin:
        if len(x)==1: return None        # (begin) => None
        else: return [expand(xi, toplevel) for xi in x]
    elif x[0] is _lambda:                # (lambda (x) e1 e2) 
        require(x, len(x)>=3)            #  => (lambda (x) (begin e1 e2))
        vars, body = x[1], x[2:]
        require(x, (isinstance(vars, list) and all(isinstance(v, Symbol) for v in vars))
                or isinstance(vars, Symbol), "illegal lambda argument list")
        exp = body[0] if len(body) == 1 else [_begin] + body
        return [_lambda, vars, expand(exp)]   
    elif x[0] is _quasiquote:            # `x => expand_quasiquote(x)
        require(x, len(x)==2)
        return expand_quasiquote(x[1])
    elif isinstance(x[0], Symbol) and x[0] in macro_table:
        return expand(macro_table[x[0]](*x[1:]), toplevel) # (m arg...) 
    else:                                #        => macroexpand if m isinstance macro
        return list(map(expand, x))            # (f arg...) => expand each

def require(x, predicate, msg="wrong length"):
    "Signal a syntax error if predicate is false."
    if not predicate: raise SyntaxError(to_string(x)+': '+msg)

def _splice(x, y):
    "Splice x (some list) into y"
    return x + list(y)

def expand_quasiquote(x):
    """Expand `x => 'x; `,x => x; `(,@x y) => (append x y) """
    if not is_pair(x):
        return [_quote, x]
    require(x, x[0] is not _unquotesplicing, "can't splice here")
    if x[0] is _unquote:
        require(x, len(x)==2)
        return x[1]
    elif is_pair(x[0]) and x[0][0] is _unquotesplicing:
        require(x[0], len(x[0])==2)
        return [_splice, x[0][1], expand_quasiquote(x[1:])]
    else:
        return [_cons, expand_quasiquote(x[0]), expand_quasiquote(x[1:])]



def let(*args):
    args = list(args)
    x = (_let, args)
    require(x, len(args)>1)
    bindings, body = args[0], args[1:]
    require(x, all(isinstance(b, list) and len(b)==2 and isinstance(b[0], Symbol)
                   for b in bindings), "illegal binding list")
    vars, vals = zip(*bindings)
    return [[_lambda, list(vars)]+ list(map(expand, body))] + list(map(expand, vals))

_append, _cons, _let = map(Sym, "append cons let".split())

macro_table = {_let:let} ## More macros can go here

eval(parse("""(begin

(define-macro and (lambda args 
   (if (null? args) #t
       (if (= (length args) 1) (car args)
           `(if ,(car args) (and ,@(cdr args)) #f)))))

;; More macros can go here
(load "sandbox.lispy")
(load "combinators.lispy")
(load "annealing.lispy")

(define (reload) (load "annealing.lispy"))
)"""))

# eval(parse('(load "sandbox.lispy")'))


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Entering repl")
        repl()

    filename = sys.argv[1]
    with open(filename, "r") as file:
        program = file.read()
    result = eval(parse(program))
    print(to_string(result))


# TODO fix strings
# TODO add &optional parameters