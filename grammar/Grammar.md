# A BNF Grammar for the Miranda Programming Language

## From the Miranda manual

Copy+Pasted from the Miranda manual.

### Syntax of Miranda scripts and expressions

```EBNF
Syntax of Miranda scripts and expressions

script = decl*

decl = def
       tdef
       spec
       libdir

def = fnform = rhs
      pat = rhs

tdef = tform '==' type(;)
       tform '::=' constructs(;)
       'abstype' tform-list 'with' sig(;)

spec = var-list '::' type(;)
       tform-list '::' 'type'(;)

sig = spec spec*

constructs = construct | constructs
             construct

construct = constructor argtype*
            type $constructor type
            '(' construct ')' argtype*

type = argtype
       typename argtype*
       type '->' type
       type $typename type

argtype = typename
          typevar
          '(' type-list? ')'
          '[' type-list ']'

tform = typename typevar*
        typevar $typename typevar

fnform = var formal*
         pat $var pat
         '(' fnform ')' formal*

pat = formal
      '-'numeral
      constructor formal*
      pat ':' pat
      pat '+' nat
      pat $constructor pat
      '(' pat ')' formal*

formal = var
         constructor
         literal1
         '(' pat-list? ')'
         '[' pat-list? ']'

rhs = simple_rhs(;)
      cases

simple_rhs = exp whdefs?

cases = alt(;) = cases
        lastcase(;)

alt = exp ',' 'if'? exp

lastcase = lastalt whdefs?

lastalt = exp ',' 'if'? exp
          exp ',' 'otherwise'

whdefs = 'where' def def*

exp = e1
      prefix1
      infix

e1 = simple simple*
     prefix e1
     e1 infix e1

simple = var
         constructor
         literal
         'readvals'
         'show'
         '(' infix1 e1 ')'
         '(' e1 infix ')'
         '(' exp-list? ')'
         '[' exp-list? ']'
         '[' exp '..' exp? ']'
         '[' exp ',' exp '..' exp? ']'
         '[' exp '|' qualifs ']'
         '[' exp '//' qualifs ']'

qualifs = qualifier ';' qualifs
          qualifier

qualifier = exp
           generator

generator = pat-list '<-' exp
            pat '<-' exp ',' exp '..'

var = identifier

constructor = IDENTIFIER

typename = identifier



```

###  Miranda lexical syntax

In this section square brackets are used to enclose a  set  of  literal
characters,  using lex-style conventions, so eg `[a-z]` means a lower case
letter.   As  usual  `*`  and  `?`   are  used  to  mean  zero-or-more,  and
zero-or-one,  occurrences of the preceding entity.  Parentheses are used
for grouping, and subtraction of one syntactic entity from another means
set  difference.   We  also  revert to using `|` for alternatives, as in
standard BNF.

```
script = (token | layout)* ;

layout = nl | tab | formfeed | space | comment ;

comment = vertical_bar vertical_bar (any - nl)* nl ;

token = identifier | IDENTIFIER | literal | typevar | delimiter ;

identifier = ([a-z] [a-zA-Z0-9_']* ) -  delimiter ;

IDENTIFIER = [A-Z] [a-zA-Z0-9_']* ;

literal = numeral | charconst | stringconst ;

literal1 = literal - float ;

numeral = nat | float ;

nat = [0-9] [0-9]* ;

float =  [0-9]* [.] nat epart? | nat epart ;

epart = [e] [+|-]? nat ;

charconst = ['] (visible-[\]|escape) ['] ;

stringconst = ["] (visible-[\"]|escape)* ["] ;

escape = [\] ([ntfrb\'"]|nl|ascii_code) ;

typevar = [*][*]* ;

delimiter = - | prefix1 | infix1 | other ;

infix1 = ++ | -- | : | \/ | & | > | >= | = | ~= | <= | < | + | *
       | / | div | mod | ^ | . | ! | $identifier | $IDENTIFIER ;

infix = infix1 | - ;

prefix1 = ~ | # ;

prefix = prefix1 | - ;

other = abstype | if | otherwise | readvals | show | type | where
       | with | %export | %free | %include | %insert | %list | %nolist
       | = | == | ::= | :: | => | vertical_bar | // | -> | \; | , | (
       | ) | [ | ] | { | } | <- | .. | $$ | $- | $+ | $* ;

vertical_bar = '|' ;
```

**Notes**

visible means any  non-control  character,  including  space  (but  not
including eg newline), nl means literal newline, and ascii_code is a `nat`
in the range `0..255` (maximum length 3 digits).

Notice that the syntax of `numeral` does not include  negative  numbers.
Negative  constants,  such  as  `-3` or `-5.05e-17` are parsed by Miranda as
applications of the prefix operator `-` to a positive numeral.  This has
no semantic significance.

Omission - the definition of `layout` does not  include  the  additional
comment rules for LITERATE SCRIPTS (see separate manual section).

## Without Single Character Tokens Expanded

```
$accept : entity
entity : error
entity : script
entity : VALUE exp
entity : EVAL exp
entity : EVAL exp COLONCOLON
entity : EVAL exp TO
script :
script : defs
exp : op
exp : e1
op : '~'
op : '#'
op : diop
diop : '-'
diop : diop1
diop1 : '+'
diop1 : PLUSPLUS
diop1 : ':'
diop1 : MINUSMINUS
diop1 : VEL
diop1 : '&'
diop1 : relop
diop1 : '*'
diop1 : '/'
diop1 : DIV
diop1 : REM
diop1 : '^'
diop1 : '.'
diop1 : '!'
diop1 : INFIXNAME
diop1 : INFIXCNAME
relop : '>'
relop : GE
relop : eqop
relop : NE
relop : LE
relop : '<'
eqop : EQEQ
eqop : '='
rhs : cases WHERE ldefs
rhs : exp WHERE ldefs
rhs : exp
rhs : cases
cases : exp ',' if exp
cases : exp ',' OTHERWISE
cases : cases reindent ELSEQ alt
alt : here exp
alt : here exp ',' if exp
alt : here exp ',' OTHERWISE
if :
if : IF
indent :
outdent : separator
separator : OFFSIDE
separator : ';'
reindent :
liste : exp
liste : liste ',' exp
e1 : '~' e1
e1 : e1 PLUSPLUS e1
e1 : e1 ':' e1
e1 : e1 MINUSMINUS e1
e1 : e1 VEL e1
e1 : e1 '&' e1
e1 : reln
e1 : e2
es1 : '~' e1
es1 : e1 PLUSPLUS e1
es1 : e1 PLUSPLUS
es1 : e1 ':' e1
es1 : e1 ':'
es1 : e1 MINUSMINUS e1
es1 : e1 MINUSMINUS
es1 : e1 VEL e1
es1 : e1 VEL
es1 : e1 '&' e1
es1 : e1 '&'
es1 : relsn
es1 : es2
e2 : '-' e2
e2 : '#' e2
e2 : e2 '+' e2
e2 : e2 '-' e2
e2 : e2 '*' e2
e2 : e2 '/' e2
e2 : e2 DIV e2
e2 : e2 REM e2
e2 : e2 '^' e2
e2 : e2 '.' e2
e2 : e2 '!' e2
e2 : e3
es2 : '-' e2
es2 : '#' e2
es2 : e2 '+' e2
es2 : e2 '+'
es2 : e2 '-' e2
es2 : e2 '-'
es2 : e2 '*' e2
es2 : e2 '*'
es2 : e2 '/' e2
es2 : e2 '/'
es2 : e2 DIV e2
es2 : e2 DIV
es2 : e2 REM e2
es2 : e2 REM
es2 : e2 '^' e2
es2 : e2 '^'
es2 : e2 '.' e2
es2 : e2 '.'
es2 : e2 '!' e2
es2 : e2 '!'
es2 : es3
e3 : comb INFIXNAME e3
e3 : comb INFIXCNAME e3
e3 : comb
es3 : comb INFIXNAME e3
es3 : comb INFIXNAME
es3 : comb INFIXCNAME e3
es3 : comb INFIXCNAME
es3 : comb
comb : comb arg
comb : arg
reln : e2 relop e2
reln : reln relop e2
relsn : e2 relop e2
relsn : e2 relop
relsn : reln relop e2
$$1 :
arg : $$1 LEX lexrules ENDIR
arg : NAME
arg : CNAME
arg : CONST
arg : READVALSY
arg : SHOWSYM
arg : DOLLAR2
arg : '[' ']'
arg : '[' exp ']'
arg : '[' exp ',' exp ']'
arg : '[' exp ',' exp ',' liste ']'
arg : '[' exp DOTDOT exp ']'
arg : '[' exp DOTDOT ']'
arg : '[' exp ',' exp DOTDOT exp ']'
arg : '[' exp ',' exp DOTDOT ']'
arg : '[' exp '|' qualifiers ']'
arg : '[' exp DIAG qualifiers ']'
arg : '(' op ')'
arg : '(' es1 ')'
arg : '(' diop1 e1 ')'
arg : '(' ')'
arg : '(' exp ',' liste ')'
$$2 :
$$3 :
lexrules : lexrules lstart here re indent $$2 ARROW exp lpostfix $$3 outdent
lexrules : lexdefs
lstart :
lstart : '<' cnames '>'
cnames : CNAME
cnames : cnames CNAME
lpostfix :
lpostfix : LBEGIN CNAME
lpostfix : LBEGIN CONST
lexdefs : lexdefs LEXDEF indent '=' re outdent
lexdefs :
re : re1 '|' re
re : re1
re1 : lterm '/' lterm
re1 : lterm '/'
re1 : lterm
lterm : lfac lterm
lterm : lfac
lfac : lunit '*'
lfac : lunit '+'
lfac : lunit '?'
lfac : lunit
lunit : '(' re ')'
lunit : CONST
lunit : CHARCLASS
lunit : ANTICHARCLASS
lunit : '.'
lunit : name
name : NAME
name : CNAME
qualifiers : exp
qualifiers : generator
qualifiers : qualifiers ';' generator
qualifiers : qualifiers ';' exp
generator : e1 ',' generator
generator : generator1
generator1 : e1 LEFTARROW exp
generator1 : e1 LEFTARROW exp ',' exp DOTDOT
defs : def
defs : defs def
def : v act2 indent '=' here rhs outdent
def : spec
def : ABSTYPE here typeforms indent WITH lspecs outdent
def : typeform indent act1 here EQEQ type act2 outdent
def : typeform indent act1 here COLON2EQ construction act2 outdent
def : indent setexp EXPORT parts outdent
def : FREE here '{' specs '}'
def : INCLUDE bindings modifiers outdent
$$4 :
def : here BNF $$4 names outdent productions ENDIR
setexp : here
bindings :
bindings : '{' bindingseq '}'
bindingseq : bindingseq binding
bindingseq : binding
binding : NAME indent '=' exp outdent
binding : typeform indent act1 EQEQ type act2 outdent
modifiers :
modifiers : negmods
negmods : negmods negmod
negmods : negmod
negmod : NAME '/' NAME
negmod : CNAME '/' CNAME
negmod : '-' NAME
here :
act1 :
act2 :
ldefs : ldef
ldefs : ldefs ldef
ldef : spec
ldef : typeform here EQEQ
ldef : typeform here COLON2EQ
ldef : v act2 indent '=' here rhs outdent
vlist : v
vlist : vlist ',' v
v : v1
v : v1 ':' v
v1 : v1 '+' CONST
v1 : '-' CONST
v1 : v2 INFIXNAME v1
v1 : v2 INFIXCNAME v1
v1 : v2
v2 : v3
v2 : v2 v3
v3 : NAME
v3 : CNAME
v3 : CONST
v3 : '[' ']'
v3 : '[' vlist ']'
v3 : '(' ')'
v3 : '(' v ')'
v3 : '(' v ',' vlist ')'
type : type1
type : type ARROW type
type1 : type2 INFIXNAME type1
type1 : type2
type2 : tap
type2 : argtype
tap : NAME argtype
tap : tap argtype
argtype : NAME
argtype : typevar
argtype : '(' typelist ')'
argtype : '[' type ']'
argtype : '[' type ',' typel ']'
typelist :
typelist : type
typelist : type ',' typel
typel : type
typel : typel ',' type
parts : parts NAME
parts : parts '-' NAME
parts : parts PATHNAME
parts : parts '+'
parts : NAME
parts : '-' NAME
parts : PATHNAME
parts : '+'
specs : specs spec
specs : spec
spec : typeforms indent here COLONCOLON ttype outdent
lspecs : lspecs lspec
lspecs : lspec
$$5 :
lspec : namelist indent here $$5 COLONCOLON type outdent
namelist : NAME ',' namelist
namelist : NAME
typeforms : typeforms ',' typeform act2
typeforms : typeform act2
typeform : CNAME typevars
typeform : NAME typevars
typeform : typevar INFIXNAME typevar
typeform : typevar INFIXCNAME typevar
ttype : type
ttype : TYPE
typevar : '*'
typevar : TYPEVAR
typevars :
typevars : typevar typevars
construction : constructs
constructs : construct
constructs : constructs '|' construct
construct : field here INFIXCNAME field
construct : construct1
construct1 : '(' construct ')'
construct1 : construct1 field1
construct1 : here CNAME
field : type
field : argtype '!'
field1 : argtype '!'
field1 : argtype
names :
names : names NAME
productions : lspec
productions : production
productions : productions lspec
productions : productions production
production : NAME params ':' indent grhs outdent
params :
$$6 :
params : $$6 '(' names ')'
grhs : here phrase
phrase : error_term
phrase : phrase1
phrase : phrase1 '|' error_term
phrase1 : term
phrase1 : phrase1 '|' here term
term : count_factors
$$7 :
term : count_factors $$7 indent '=' here rhs outdent
error_term : ERRORSY
$$8 :
error_term : ERRORSY $$8 indent '=' here rhs outdent
count_factors : EMPTYSY
count_factors : EMPTYSY factors
$$9 :
count_factors : $$9 factors
factors : factor
factors : factors factor
factor : unit
factor : '{' unit '}'
factor : '{' unit
factor : unit '}'
unit : symbol
unit : symbol '*'
unit : symbol '+'
unit : symbol '?'
symbol : NAME
symbol : ENDSY
symbol : CONST
symbol : '^'
$$10 :
$$11 :
symbol : $$10 '[' exp $$11 ']'
symbol : '-'
```

## With Single Character Tokens Expanded

```
$accept : entity
entity : error
entity : script
entity : Value exp
entity : Eval exp
entity : Eval exp ColonColon
entity : Eval exp To
script :
script : defs
exp : op
exp : e1
op : Tilde
op : Hash
op : diop
diop : Minus
diop : diop1
diop1 : Plus
diop1 : PlusPlus
diop1 : Colon
diop1 : MinusMinus
diop1 : Vel
diop1 : Ampersand
diop1 : relop
diop1 : Asterisk
diop1 : Slash
diop1 : Divide
diop1 : Remainder
diop1 : Caret
diop1 : Dot
diop1 : Bang
diop1 : InfixName
diop1 : InfixCName
relop : Greater
relop : GreaterEqual
relop : eqop
relop : NotEqual
relop : LessEqual
relop : Less
eqop : EqualEqual
eqop : Equal
rhs : cases Where ldefs
rhs : exp Where ldefs
rhs : exp
rhs : cases
cases : exp Comma if exp
cases : exp Comma Otherwise
cases : cases reindent ElseEqual alt
alt : here exp
alt : here exp Comma if exp
alt : here exp Comma Otherwise
if :
if : If
indent :
outdent : separator
separator : Offside
separator : Semicolon
reindent :
liste : exp
liste : liste Comma exp
e1 : Tilde e1
e1 : e1 PlusPlus e1
e1 : e1 Colon e1
e1 : e1 MinusMinus e1
e1 : e1 Vel e1
e1 : e1 Ampersand e1
e1 : reln
e1 : e2
es1 : Tilde e1
es1 : e1 PlusPlus e1
es1 : e1 PlusPlus
es1 : e1 Colon e1
es1 : e1 Colon
es1 : e1 MinusMinus e1
es1 : e1 MinusMinus
es1 : e1 Vel e1
es1 : e1 Vel
es1 : e1 Ampersand e1
es1 : e1 Ampersand
es1 : relsn
es1 : es2
e2 : Minus e2
e2 : Hash e2
e2 : e2 Plus e2
e2 : e2 Minus e2
e2 : e2 Asterisk e2
e2 : e2 Slash e2
e2 : e2 Divide e2
e2 : e2 Remainder e2
e2 : e2 Caret e2
e2 : e2 Dot e2
e2 : e2 Bang e2
e2 : e3
es2 : Minus e2
es2 : Hash e2
es2 : e2 Plus e2
es2 : e2 Plus
es2 : e2 Minus e2
es2 : e2 Minus
es2 : e2 Asterisk e2
es2 : e2 Asterisk
es2 : e2 Slash e2
es2 : e2 Slash
es2 : e2 Divide e2
es2 : e2 Divide
es2 : e2 Remainder e2
es2 : e2 Remainder
es2 : e2 Caret e2
es2 : e2 Caret
es2 : e2 Dot e2
es2 : e2 Dot
es2 : e2 Bang e2
es2 : e2 Bang
es2 : es3
e3 : comb InfixName e3
e3 : comb InfixCName e3
e3 : comb
es3 : comb InfixName e3
es3 : comb InfixName
es3 : comb InfixCName e3
es3 : comb InfixCName
es3 : comb
comb : comb arg
comb : arg
reln : e2 relop e2
reln : reln relop e2
relsn : e2 relop e2
relsn : e2 relop
relsn : reln relop e2
$$1 :
arg : $$1 Lex lexrules EndIR
arg : Name
arg : ConstructorName
arg : Constant
arg : ReadVal
arg : Show
arg : DollarDollar
arg : OpenBracket CloseBracket
arg : OpenBracket exp CloseBracket
arg : OpenBracket exp Comma exp CloseBracket
arg : OpenBracket exp Comma exp Comma liste CloseBracket
arg : OpenBracket exp DotDot exp CloseBracket
arg : OpenBracket exp DotDot CloseBracket
arg : OpenBracket exp Comma exp DotDot exp CloseBracket
arg : OpenBracket exp Comma exp DotDot CloseBracket
arg : OpenBracket exp Pipe qualifiers CloseBracket
arg : OpenBracket exp Diagonal qualifiers CloseBracket
arg : OpenParenthesis op CloseParenthesis
arg : OpenParenthesis es1 CloseParenthesis
arg : OpenParenthesis diop1 e1 CloseParenthesis
arg : OpenParenthesis CloseParenthesis
arg : OpenParenthesis exp Comma liste CloseParenthesis
$$2 :
$$3 :
lexrules : lexrules lstart here re indent $$2 Arrow exp lpostfix $$3 outdent
lexrules : lexdefs
lstart :
lstart : Less cnames Greater
cnames : ConstructorName
cnames : cnames ConstructorName
lpostfix :
lpostfix : Begin ConstructorName
lpostfix : Begin Constant
lexdefs : lexdefs LexDef indent Equal re outdent
lexdefs :
re : re1 Pipe re
re : re1
re1 : lterm Slash lterm
re1 : lterm Slash
re1 : lterm
lterm : lfac lterm
lterm : lfac
lfac : lunit Asterisk
lfac : lunit Plus
lfac : lunit QuestionMark
lfac : lunit
lunit : OpenParenthesis re CloseParenthesis
lunit : Constant
lunit : CharClass
lunit : AntiCharClass
lunit : Dot
lunit : name
name : Name
name : ConstructorName
qualifiers : exp
qualifiers : generator
qualifiers : qualifiers Semicolon generator
qualifiers : qualifiers Semicolon exp
generator : e1 Comma generator
generator : generator1
generator1 : e1 LeftArrow exp
generator1 : e1 LeftArrow exp Comma exp DotDot
defs : def
defs : defs def
def : v act2 indent Equal here rhs outdent
def : spec
def : AbsoluteType here typeforms indent With lspecs outdent
def : typeform indent act1 here EqualEqual type act2 outdent
def : typeform indent act1 here Colon2Equal construction act2 outdent
def : indent setexp Export parts outdent
def : Free here OpenBrace specs CloseBrace
def : Include bindings modifiers outdent
$$4 :
def : here BNF $$4 names outdent productions EndIR
setexp : here
bindings :
bindings : OpenBrace bindingseq CloseBrace
bindingseq : bindingseq binding
bindingseq : binding
binding : Name indent Equal exp outdent
binding : typeform indent act1 EqualEqual type act2 outdent
modifiers :
modifiers : negmods
negmods : negmods negmod
negmods : negmod
negmod : Name Slash Name
negmod : ConstructorName Slash ConstructorName
negmod : Minus Name
here :
act1 :
act2 :
ldefs : ldef
ldefs : ldefs ldef
ldef : spec
ldef : typeform here EqualEqual
ldef : typeform here Colon2Equal
ldef : v act2 indent Equal here rhs outdent
vlist : v
vlist : vlist Comma v
v : v1
v : v1 Colon v
v1 : v1 Plus Constant
v1 : Minus Constant
v1 : v2 InfixName v1
v1 : v2 InfixCName v1
v1 : v2
v2 : v3
v2 : v2 v3
v3 : Name
v3 : ConstructorName
v3 : Constant
v3 : OpenBracket CloseBracket
v3 : OpenBracket vlist CloseBracket
v3 : OpenParenthesis CloseParenthesis
v3 : OpenParenthesis v CloseParenthesis
v3 : OpenParenthesis v Comma vlist CloseParenthesis
type : type1
type : type Arrow type
type1 : type2 InfixName type1
type1 : type2
type2 : tap
type2 : argtype
tap : Name argtype
tap : tap argtype
argtype : Name
argtype : typevar
argtype : OpenParenthesis typelist CloseParenthesis
argtype : OpenBracket type CloseBracket
argtype : OpenBracket type Comma typel CloseBracket
typelist :
typelist : type
typelist : type Comma typel
typel : type
typel : typel Comma type
parts : parts Name
parts : parts Minus Name
parts : parts PathName
parts : parts Plus
parts : Name
parts : Minus Name
parts : PathName
parts : Plus
specs : specs spec
specs : spec
spec : typeforms indent here ColonColon ttype outdent
lspecs : lspecs lspec
lspecs : lspec
$$5 :
lspec : namelist indent here $$5 ColonColon type outdent
namelist : Name Comma namelist
namelist : Name
typeforms : typeforms Comma typeform act2
typeforms : typeform act2
typeform : ConstructorName typevars
typeform : Name typevars
typeform : typevar InfixName typevar
typeform : typevar InfixCName typevar
ttype : type
ttype : Type
typevar : Asterisk
typevar : TypeVar
typevars :
typevars : typevar typevars
construction : constructs
constructs : construct
constructs : constructs Pipe construct
construct : field here InfixCName field
construct : construct1
construct1 : OpenParenthesis construct CloseParenthesis
construct1 : construct1 field1
construct1 : here ConstructorName
field : type
field : argtype Bang
field1 : argtype Bang
field1 : argtype
names :
names : names Name
productions : lspec
productions : production
productions : productions lspec
productions : productions production
production : Name params Colon indent grhs outdent
params :
$$6 :
params : $$6 OpenParenthesis names CloseParenthesis
grhs : here phrase
phrase : error_term
phrase : phrase1
phrase : phrase1 Pipe error_term
phrase1 : term
phrase1 : phrase1 Pipe here term
term : count_factors
$$7 :
term : count_factors $$7 indent Equal here rhs outdent
error_term : ErrorSymbol
$$8 :
error_term : ErrorSymbol $$8 indent Equal here rhs outdent
count_factors : EmptySymbol
count_factors : EmptySymbol factors
$$9 :
count_factors : $$9 factors
factors : factor
factors : factors factor
factor : unit
factor : OpenBrace unit CloseBrace
factor : OpenBrace unit
factor : unit CloseBrace
unit : symbol
unit : symbol Asterisk
unit : symbol Plus
unit : symbol QuestionMark
symbol : Name
symbol : EndSymbol
symbol : Constant
symbol : Caret
$$10 :
$$11 :
symbol : $$10 OpenBracket exp $$11 CloseBracket
symbol : Minus
```
