import ply.lex as lex
import ply.yacc as yacc


initials=["ICST","LB","RB","ARROW","COMMA","ID","SEM_COL_PYTH","ASSIGN"]

reserved ={"markov":"MARKOV","path":"PATH","_":"DONTCARE","const":"CONST"}

t_LB=r"\{"
t_RB=r"\}"
t_COMMA=","
t_ARROW="->"



tokens=initials+list(reserved.values())

def t_SEM_COL_PYTH(t):
    ":.*"
    t.value=t.value[1:].strip()
    return t

def t_ASSIGN(t):
    "=.*"
    t.value=t.value[1:].strip()
    return t

def t_ICST(t):
    r"[1-9][0-9]*|0"
    t.value=int(t.value)
    return t

def t_ID(t):
    r"[a-zA-Z_][a-zA-Z_0-9]*"
    if t.value in reserved:
        t.type=reserved[t.value]
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

t_ignore=" \t\r"

t_ignore_COMMENT=r"\#.*"

def t_error(t):
    raise ValueError(f"Illegal character {t.value[0]}")

lexer=lex.lex()


################## SYNTAX ##################

def p_start(p):
    """s : MARKOV ID LB definitions RB paths
    """
    result = {'markov' :p[2],"definitions" :p[4],"paths" :p[6]}
    p[0] = result

def p_definitions(p):
    """definitions : definitions one_definition
    | one_definition
    """
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

def p_one_definition(p):
    """one_definition : list_state ARROW LB update RB
    | CONST ID ASSIGN
    | CONST ID
    """
    if len(p) == 6:
        p[0] = (p[1],p[4])
    elif len(p) == 3:
        p[0] = p[2]
    else:
        p[0] = [p[2],p[3]]

def p_list_state(p):
    """list_state : list_state COMMA one_state
    | one_state
    """
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]

def p_one_state(p):
    """one_state : ID
    | ICST
    """
    p[0] = p[1]

def p_update(p):
    """update : update one_update
    | one_update
    """
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = [p[1]]

def p_one_update(p):
    """one_update : list_state SEM_COL_PYTH
    """
    p[0] = {"state": p[1], "probability": p[2]}

def p_paths(p):
    """paths : paths one_path
    | one_path
    """
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = [p[1]]

def p_one_path(p):
    """one_path : PATH list_spec
    """
    p[0] = {"type": "Path", "spec": p[2]}

def p_list_spec(p):
    """list_spec : list_spec COMMA one_spec
    | one_spec
    """
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]


def p_one_spec(p):
    """one_spec : ICST
    | DONTCARE
    | ID
    """
    p[0] = p[1]

def p_error(p):
    if p:
        raise ValueError(f'Syntax error, but "{p.value}" found, line {p.lexer.lineno}')
    else:
        raise ValueError(f"Syntax error, but EOF found")

f=open("/prog.grpy")
input_=f.read()
f.close()

parser=yacc.yacc()
try:
    parser.parse(input_)
    
    print("No error")
except Exception as e:
    print(e)



def execute(ast, locals, path):
    transition_matrix = find_transition_matrix(ast)
    probability = 1
    for i in range(len(path)-1):
        probability *= transition_matrix[path[i]][path[i+1]]
    return probability

def find_transition_matrix(ast):
    transition_matrix = {}
    # Traverse the AST and extract the transition probabilities
    for node in ast:
        if node[0] == "one_update":
            # Extract the list of states and the probability expression
            list_state = node[1]
            sem_col_pyth = node[2]
            # evaluate the probability expression using eval
            probability = eval(sem_col_pyth, {}, locals)
            for i in range(len(list_state)-1):
                if list_state[i][1] not in transition_matrix:
                    transition_matrix[list_state[i][1]] = {}
                transition_matrix[list_state[i][1]][list_state[i+1][1]] = probability
    if transition_matrix:
        return transition_matrix
    else:
        raise ValueError("No Transition matrix defined in the source code")


# Example usage:
ast = parser.parse(input_)
locals = {"q": 0.2}
path = ["st0", "st0", "st1", "st2"]
prob = execute(ast, locals, path)
print(f"Probability of path {path}: {prob}")
