/*
    Bison specification file for a parser for Miranda written in Rust.

*/

%define api.parser.struct {Parser}
%define api.value.type {Value}
%define api.parser.check_debug { self.debug }

%define parse.error custom
%define parse.trace

%code use {
// all use goes here
use super::{
  token::Token as LexerToken,
  token_wrapper::TokenWrapper as Token,
  lexer::Lexer,
  error::Error,
  Loc
};

use crate::data::values
}

%code parser_fields {
  /// Gives access to the end result of parsing.
  result: Option<Value>,
  /// Enables debug printing
  pub debug: bool,
  /// The heap on which everything is constructed.
  heap: &mut Heap,
}

%code {
  // code in a generic %code block.
}

%token Value Eval Where If To LeftArrow ColonColon Colon2Equal Identifier
    TypeVar Name Constant ConstructorName DollarDollar Offside ElseEqual
    AbsoluteType With Diagonal EqualEqual Free Include Export Type Otherwise
    Show PathName BNF Lex EndIR ErrorSymbol EndSymbol EmptySymbol ReadVals LexDef
    CharClass AntiCharClass Begin RightArrow PlusPlus MinusMinus DotDot Vel
    GreaterEqual NotEqual LessEqual Remainder IntegerDivide InfixName
    InfixCName Colon Ampersand Greater Equal Less Plus Minus Times Divide
    Caret Dot Bang Tilde Hash Comma Pipe QuestionMark Semicolon Newline
    OpenBrace CloseBrace OpenParenthesis CloseParenthesis OpenBracket
    CloseBracket StringLiteral Integer Float

%right Arrow
%right PlusPlus Colon MinusMinus
%nonassoc DotDot
%right Vel
%right Ampersand
%nonassoc Greater GreaterEqual Equal NotEqual LessEqual Less
%left Plus Minus
%left Times IntegerDivide Remainder Divide
%right Caret
%left Dot   /* ?? fiddle to make Hash behave */
%left Bang
%right InfixName InfixCName
//%token CMBASE  /* Unnecessary: placeholder to start combinator values */

%{
/*
Miranda has text representation of tokens here, but we don't need that,
because we defined them in `Token::name()`.
    char *yysterm[]= {…}


Miranda has all of this global state:

  extern word listdiff_fn, indent_fn, outdent_fn;
  word lastname=0;
  word suppressids=NIL;
  word idsused=NIL;
  word tvarscope=0;
  word includees=NIL, embargoes=NIL, exportfiles=NIL, freeids=NIL, exports=NIL;
  word lexdefs=NIL, lexstates=NIL, inlex=0, inexplist=0;
  word inbnf=0, col_fn=0, fnts=NIL, eprodnts=NIL, nonterminals=NIL, sreds=0;
  word ihlist=0, ntspecmap=NIL, ntmap=NIL, lasth=0;
  word obrct=0;



Miranda uses the following utility functions:
    void evaluate(word x);
    // like evaluate but no fork, no stats, no extra '\n'
    void obey(word x);
    int isstring(word x);
    // used in compiling 'cases'
    word compose(word x);
    int eprod(word);
    // x is grammar rhs - returns list of nonterminals in start set
    word starts(word x);
    // x is grammar rhs - does x admit empty production?
    int eprod(wordx);
    // clumsy - this algorithm is quadratic in number of prodns - fix later
    word add_prod(word d, word ps, word hr);
    // get here info for nonterminal
    word getloc(word nt, word prods);
    // set errs to here info of undefined nonterminal
    void findnt(word nt);
    //  performs the binomial optimisation on rhs of nonterminal x
    //     x: x alpha1| ... | x alphaN | rest     ||need not be in this order
    //         ==>
    //     x: rest (alpha1|...|alphaN)*
    //
    // should put some labels on the alpha's - fix later
    void binom(word rhs, word x);
    word getcol_fn();
    void startbnf();
    // abstract inherited attributes from grammar rule
    word ih_abstr(word x);
    // is x of the form $1 applied to ih attributes in order?
    int can_elide(word x);
    // does regular expression x match empty string ?
    int e_re(word x);
*/
%}

%%

entity:  /* the entity to be parsed is either a definition script or an
            expression (the latter appearing as a command line) */

    error

    | script {
        // Done compiling script in this action.
        // lastname=0;
        /* outstats(); */  // statistics not usually wanted after compilation
      }

          /*
          The following relate to interactive sessions. See
              https://www.cs.kent.ac.uk/people/staff/dat/miranda/manual/4.html
          for the different commands for interactive sessions.
          */
    | Value exp {
        /*
        In an interactive session, the special variable `$$` always
        contains the value of the last evaluated expression. Since we
        separate parsing from evaluation, we don't set this value here.
        */
        // lastexp=$2; // next line of `$+'
      }

    | Eval exp {
        // If no sytntax errors and the last token read from the lexer was
        // EOF, then evaluate the expression `exp`. Moved out of the parser.
      }

    | Eval exp ColonColon {
        // Typecheck `exp`.
      }

    | Eval exp To {
          /* exp &> pathname
          A background process is set up  to  evaluate  exp,  and  the  resulting
          output (including error messages, if any) sent to the designated file.
          */
      }
    ;

script:
    /* script can be empty */
    | defs
    ;

exp:
    op /* will later suppress in favour of (op) in arg */
    | e1
    ;

op:
    Tilde { $$ = Combinator::Not.into(); }
    | Hash { $$ = Combinator::Length.into(); }
    | diop
    ;

diop:
    Minus { $$ = Combinator::Minus.into(); }
    | diop1
    ;

diop1:
    Plus { $$ = Combinator::Plus.into(); }
    | PlusPlus { $$ = Combinator::Append.into(); }
    | Colon { $$ = Combinator::P.into(); }
    | MinusMinus { $$ = listdiff_fn; }
    | Vel { $$ = Combinator::OR.into(); }
    | Ampersand { $$ = Combinator::And.into(); }
    | relop
    | Times { $$ = Combinator::Times.into(); }
    | Divide { $$ = Combinator::Divide.into(); }
    | IntegerDivide { $$ = Combinator::IntegerDivide.into(); }
    | Remainder { $$ = Combinator::Remainder.into(); }
    | Caret { $$ = Combinator::Power.into(); }
    | Dot { $$ = Combinator::B.into(); }
    | Bang { $$ = ap(Combinator::C.into(), Combinator::Subscript.into(); }
    | InfixName
    | InfixCName
    ;

relop:
    Greater { $$ = GR; }
    | GreaterEqual { $$ = GRE; }
    | eqop { $$ = EQ; }
    | NotEqual { $$ = NEQ; }
    | LessEqual { $$ = ap(C, GRE); }
    | Less { $$ = ap(C, GR); }
    ;

eqop:
    EqualEqual /* silently accept for benefit of Haskell users */
    | Equal
    ;

rhs:
    cases Where ldefs { $$ = block($3, compose($1), 0); }
    | exp Where ldefs { $$ = block($3, $1, 0); }
    | exp
    | cases { $$ = compose($1); }
    ;

cases:
    exp Comma if exp { $$ = cons(ap2(COND, $4, $1), NIL); }
    | exp Comma Otherwise { $$ = cons(ap(Otherwise, $1), NIL); }
    | cases reindent ElseEqual alt {
        $$ = cons($4, $1);
        if(hd[hd[$1]]==Otherwise){
          syntax("\"otherwise\" must be last case\n");
        }
      }
    ;

alt:
    here exp {
        errs=$1;
        syntax("obsolete syntax, \", otherwise\" missing\n");
        $$ = ap(Otherwise, label($1, $2));
      }
    | here exp Comma if exp { $$ = label($1, ap2(COND, $5, $2)); }
    | here exp Comma Otherwise { $$ = ap(Otherwise, label($1, $2)); }
    ;

if:
    /* empty */ {
        //extern word strictif;
        if strictif {
          syntax("\"if\" missing\n");
        }
      }
    | If
    ;

indent:
    /* empty */ {
        if(!SYNERR){
          layout();
          setlmargin();
        }
      }
    ;
/* note that because of yacc's one symbol look ahead, indent must usually be
   invoked one symbol earlier than the non-terminal to which it applies
   - see `production:' for an exception */

outdent:
    separator { unsetlmargin(); }
    ;

separator:
    Offside
    | Semicolon
    ;

reindent:
    /* empty */ {
        if(!SYNERR) {
          unsetlmargin();
          layout();
          setlmargin();
        }
      }
    ;

liste:  /* NB - returns list in reverse order */
    exp { $$ = cons($1, NIL); }
    | liste Comma exp  /* left recursive so as not to eat YACC stack */
        { $$ = cons($3, $1); }
    ;

e1:
    Tilde e1 %prec Equal { $$ = ap(NOT, $2); }
    | e1 PlusPlus e1 { $$ = ap2(Append, $1, $3); }
    | e1 Colon e1 { $$ = cons($1, $3); }
    | e1 MinusMinus e1 { $$ = ap2(listdiff_fn, $1, $3);  }
    | e1 Vel e1 { $$ = ap2(OR, $1, $3); }
    | e1 Ampersand e1 { $$ = ap2(And, $1, $3); }
    | reln
    | e2
    ;

es1:                     /* e1 or presection */
    Tilde e1 %prec Equal { $$ = ap(NOT, $2); }
    | e1 PlusPlus e1 { $$ = ap2(Append, $1, $3); }
    | e1 PlusPlus { $$ = ap(Append, $1); }
    | e1 Colon e1 { $$ = cons($1, $3); }
    | e1 Colon { $$ = ap(P, $1); }
    | e1 MinusMinus e1 { $$ = ap2(listdiff_fn, $1, $3);  }
    | e1 MinusMinus { $$ = ap(listdiff_fn, $1);  }
    | e1 Vel e1 { $$ = ap2(OR, $1, $3); }
    | e1 Vel { $$ = ap(OR, $1); }
    | e1 Ampersand e1 { $$ = ap2(And, $1, $3); }
    | e1 Ampersand { $$ = ap(And, $1); }
    | relsn
    | es2
    ;

e2:
    Minus e2 %prec Minus { $$ = ap(NEG, $2); }
    | Hash e2 %prec Dot { $$ = ap(LENGTH, $2);  }
    | e2 Plus e2 { $$ = ap2(Plus, $1, $3); }
    | e2 Minus e2 { $$ = ap2(Minus, $1, $3); }
    | e2 Times e2 { $$ = ap2(Times, $1, $3); }
    | e2 Divide e2 { $$ = ap2(Divide, $1, $3); }
    | e2 IntegerDivide e2 { $$ = ap2(IntegerDivide, $1, $3); }
    | e2 Remainder e2 { $$ = ap2(Remainder, $1, $3); }
    | e2 Caret e2 { $$ = ap2(Power, $1, $3); }
    | e2 Dot e2 { $$ = ap2(B, $1, $3);  }
    | e2 Bang e2 { $$ = ap2(SUBSCRIPT, $3, $1); }
    | e3
    ;

es2:               /* e2 or presection */
    Minus e2 %prec Minus { $$ = ap(NEG, $2); }
    | Hash e2 %prec Dot { $$ = ap(LENGTH, $2);  }
    | e2 Plus e2 { $$ = ap2(Plus, $1, $3); }
    | e2 Plus { $$ = ap(Plus, $1); }
    | e2 Minus e2 { $$ = ap2(Minus, $1, $3); }
    | e2 Minus { $$ = ap(Minus, $1); }
    | e2 Times e2 { $$ = ap2(Times, $1, $3); }
    | e2 Times { $$ = ap(Times, $1); }
    | e2 Divide e2 { $$ = ap2(Divide, $1, $3); }
    | e2 Divide { $$ = ap(Divide, $1); }
    | e2 IntegerDivide e2 { $$ = ap2(IntegerDivide, $1, $3); }
    | e2 IntegerDivide { $$ = ap(IntegerDivide, $1); }
    | e2 Remainder e2 { $$ = ap2(Remainder, $1, $3); }
    | e2 Remainder { $$ = ap(Remainder, $1); }
    | e2 Caret e2 { $$ = ap2(Power, $1, $3); }
    | e2 Caret { $$ = ap(Power, $1); }
    | e2 Dot e2 { $$ = ap2(B, $1, $3);  }
    | e2 Dot { $$ = ap(B, $1);  }
    | e2 Bang e2 { $$ = ap2(SUBSCRIPT, $3, $1); }
    | e2 Bang { $$ = ap2(C, SUBSCRIPT, $1); }
    | es3
    ;

e3:
    comb InfixName e3 { $$ = ap2($2, $1, $3); }
    | comb InfixCName e3 { $$ = ap2($2, $1, $3); }
    | comb
    ;

es3:                     /* e3 or presection */
    comb InfixName e3 { $$ = ap2($2, $1, $3); }
    | comb InfixName { $$ = ap($2, $1); }
    | comb InfixCName e3 { $$ = ap2($2, $1, $3); }
    | comb InfixCName { $$ = ap($2, $1); }
    | comb
    ;

comb:
    comb arg { $$ = ap($1, $2); }
    | arg
    ;

reln:
    e2 relop e2 { $$ = ap2($2, $1, $3); }
    | reln relop e2 {
        /* EFFICIENCY PROBLEM - subject gets re-evaluated (and
            retypechecked) - fix later */
        let subject = if hd[hd[$1]]==And {
          tl[tl[$1]]
        } else {
          tl[$1]
        };
        $$ = ap2(And, $1, ap2($2, subject, $3));
      }
    ;

relsn:                     /* reln or presection */
    e2 relop e2 { $$ = ap2($2, $1, $3); }
    | e2 relop { $$ = ap($2, $1); }
    | reln relop e2 {
        /* EFFICIENCY PROBLEM - subject gets re-evaluated (and
                    retypechecked) - fix later */
        let subject = if hd[hd[$1]]==And {
          tl[tl[$1]]
        } else {
          tl[$1]
        };
        $$ = ap2(And, $1, ap2($2, subject, $3));
      }
    ;

arg:
    /* empty */ {
      if !SYNERR {
        lexstates = NIL;
        inlex = 1;
      }
    }
    Lex lexrules EndIR {
          inlex = 0;
          lexdefs = NIL;
          if lexstates!=NIL {
            let echoed = 0;
            for(; lexstates!=NIL; lexstates=tl[lexstates] ) {
              if(!echoed){
                printf(echoing?"\n":"");
                echoed = 1;
              }
              if(!(tl[hd[lexstates]]&1)){
                printf(
                  "warning: lex state %s is never entered\n",
                  get_id(hd[hd[lexstates]])
                );
              } else {
                if(!(tl[hd[lexstates]]&2)) {
                  printf(
                    "warning: lex state %s has no associated rules\n",
                    get_id(hd[hd[lexstates]])
                  );
                }
              }
            }
          }
          if $3==NIL {
            syntax("%lex with no rules\n");
          } else {
            tag[$3] = LEXER;
          }
          /*
            result is lex-list, in reverse order, of items of the form
              cons(scstuff, cons(matcher, rhs))
            where scstuff is of the form
              cons(0-or-list-of-startconditions, 1+newstartcondition)
          */
          $$ = $3;
        }
    | Name
    | ConstructorName
    | Constant
    | ReadVals { $$ = readvals(0, 0); }
    | Show { $$ = show(0, 0); }
    | DollarDollar {
          $$ = lastexp;
          if lastexp==UNDEF {
            syntax("no previous expression to substitute for $$\n");
          }
        }
    | OpenBracket CloseBracket { $$ = NIL; }
    | OpenBracket exp CloseBracket { $$ = cons($2, NIL); }
    | OpenBracket exp Comma exp CloseBracket { $$ = cons($2, cons($4, NIL)); }
    | OpenBracket exp Comma exp Comma liste CloseBracket { $$ = cons($2, cons($4, reverse($6))); }
    | OpenBracket exp DotDot exp CloseBracket { $$ = ap3(STEPUNTIL, big_one, $4, $2); }
    | OpenBracket exp DotDot CloseBracket { $$ = ap2(STEP, big_one, $2); }
    | OpenBracket exp Comma exp DotDot exp CloseBracket { $$ = ap3(STEPUNTIL, ap2(Minus, $4, $2), $6, $2); }
    | OpenBracket exp Comma exp DotDot CloseBracket { $$ = ap2(STEP, ap2(Minus, $4, $2), $2); }
    | OpenBracket exp Pipe qualifiers CloseBracket {
        $$ = if SYNERR {
          NIL
        }else {
          compzf($2, $4, 0)
        };
      }
    | OpenBracket exp Diagonal qualifiers CloseBracket {
          $$ = if SYNERR {
            NIL
          } else {
            compzf($2, $4, 1)
          };
        }
    | OpenParenthesis op CloseParenthesis       /* RSB */ { $$ = $2; }
    | OpenParenthesis es1 CloseParenthesis      /* presection or parenthesised e1 */ { $$ = $2; }
    | OpenParenthesis diop1 e1 CloseParenthesis /* postsection */ {
        /* optimisation */
        $$ = if (tag[$2]==AP && hd[$2]==C){
          ap(tl[$2], $3)
        } else {
          ap2(C, $2, $3)
        };
      }
    | OpenParenthesis CloseParenthesis { $$ = Void; }  /* the void tuple */
    | OpenParenthesis exp Comma liste CloseParenthesis {
        if (tl[$4]==NIL) {
          $$ = pair($2, hd[$4]);
        } else {
          $$ = pair(hd[tl[$4]], hd[$4]);
          $4 = tl[tl[$4]];
          while ($4!=NIL) {
            $$ = tcons(hd[$4], $$);
            $4 = tl[$4];
          }
          $$ = tcons($2, $$);
        }
        /* representation of the tuple (a1, ..., an) is
            tcons(a1, tcons(a2, ...pair(a(n-1), an))) */
      }
    ;

lexrules:
    lexrules lstart here re indent { if(!SYNERR)inlex=2; }

    Arrow exp lpostfix { if(!SYNERR){ inlex=1; } } outdent {
        if ($9<0 && e_re($4)) {
          errs = $3;
          syntax("illegal lex rule - lhs matches empty\n");
        }
        $$ = cons(cons(cons($2, 1+$9), cons($4, label($3, $8))), $1);
      }
    | lexdefs { $$ = NIL; }
    ;

lstart:
    /* empty */ { $$ = 0; }
    | Less cnames Greater {
        word ns=NIL;
        for(;$2!=NIL;$2=tl[$2]) {
          let *x = &lexstates;
          let i = 1;
          while (*x != NIL && hd[hd[*x]] != hd[$2]) {
            i++;
            x = &tl[*x];
          }
          if(*x == NIL) {
            *x = cons(cons(hd[$2], 2), NIL);
          } else {
            tl[hd[*x]] |= 2;
          }
          ns = add1(i, ns);
        }
        $$ = ns;
      }
    ;

cnames:
    ConstructorName { $$=cons($1, NIL); }
    | cnames ConstructorName {
        if(member($1, $2)){
          printf(
            "%ssyntax error: repeated name \"%s\" in start conditions\n",
            if echoing {"\n"} else {""},
            get_id($2)
          );
          acterror();
        }
        $$ = cons($2, $1);
      }
    ;

lpostfix:
        /* empty */ { $$ = -1; }
        | Begin ConstructorName {
            let *x = &lexstates;
            let i=1;
            while(*x!=NIL&&hd[hd[*x]]!=$2){
              i++;
              x = &tl[*x];
            }
            if(*x == NIL){
              *x = cons(cons($2, 1), NIL);
            } else {
              tl[hd[*x]] |= 1;
            }
            $$ = i;
          }
        | Begin Constant {
            if (!isnat($2) || get_int($2)!=0) {
              syntax("%begin not followed by IDENTIFIER or 0\n");
            }
            $$ = 0;
          }
        ;

lexdefs:
    lexdefs LexDef indent Equal re outdent { lexdefs = cons(cons($2, $5), lexdefs); }
    | /* empty */ { lexdefs = NIL; }
    ;

re:   /* regular expression */
    re1 Pipe re { $$ = ap2(LEX_OR, $1, $3); }
    | re1
    ;

re1:
    lterm Divide lterm { $$ = ap2(LEX_RCONTEXT, $1, $3); }
    | lterm Divide { $$ = ap2(LEX_RCONTEXT, $1, 0); }
    | lterm
    ;

lterm:
    lfac lterm { $$ = ap2(LEX_SEQ, $1, $2); }
    | lfac
    ;

lfac:
    lunit Times {
        if (e_re($1)) {
          syntax("illegal regular expression - arg of * matches empty\n");
        }
        $$ = ap(LEX_STAR, $1);
      }
    | lunit Plus { $$ = ap2(LEX_SEQ, $1, ap(LEX_STAR, $1)); }
    | lunit QuestionMark { $$ = ap(LEX_OPT, $1); }
    | lunit
    ;

lunit:
    OpenParenthesis re CloseParenthesis { $$ = $2; }
    | Constant {
        if(!isstring($1)){
          printf(
            "%ssyntax error - unexpected token \"",
            if echoing {"\n"} else {""}
          );
          out(stdout, $1);
          printf("\" in regular expression\n");
          acterror();
        };
        $$ = if $1==NILS {
          ap(LEX_STRING, NIL)
        } else{
          if tl[$1]==NIL {
            ap(LEX_CHAR, hd[$1])
          } else{
            ap(LEX_STRING, $1)
          }
        };
      }
    | CharClass {
        if($1==NIL){
          syntax("empty character class `` cannot match\n");
        }
        $$ = if tl[$1]==NIL {
          ap(LEX_CHAR, hd[$1])
        } else {
          ap(LEX_CLASS, $1)
        };
      }
    | AntiCharClass { $$ = ap(LEX_CLASS, cons(ANTICHARCLASS, $1)); }
    | Dot { $$ = LEX_DOT; }
    | name {
        let x = lexdefs;
        while (x!=NIL && hd[hd[x]]!=$1) {
          x=tl[x];
        }
        if (x==NIL) {
          printf(
            "%ssyntax error: undefined lexeme %s in regular expression\n",
            if echoing {"\n"} else {""},
            get_id($1)
          );
          acterror();
        } else {
          $$ = tl[hd[x]];
        }
      }
    ;

name: Name | ConstructorName ;

qualifiers:
    exp { $$ = cons(cons(GUARD, $1), NIL);  }
    | generator { $$ = cons($1, NIL);  }
    | qualifiers Semicolon generator { $$ = cons($3, $1);   }
    | qualifiers Semicolon exp { $$ = cons(cons(GUARD, $3), $1);   }
    ;

generator:
    e1 Comma generator {
        /* fix syntax to disallow patlist on lhs of iterate generator */
        if (hd[$3]==GENERATOR) {
          let e = tl[tl[$3]];
          if (
            tag[e]==AP
            && tag[hd[e]]==AP
            && (hd[hd[e]]==ITERATE || hd[hd[e]]==ITERATE1)
          )
          {
            syntax("ill-formed generator\n");
          }
        }
        $$ = cons(REPEAT, cons(genlhs($1), $3));
        idsused = NIL;
      }
    | generator1
    ;

generator1:
    e1 LeftArrow exp {
        $$ = cons(GENERATOR, cons(genlhs($1), $3));
        idsused=NIL;
      }
    | e1 LeftArrow exp Comma exp DotDot {
        word p = genlhs($1);
        idsused=NIL;
        $$ = cons(
          GENERATOR,
          cons(
            p,
            ap2(
              if irrefutable(p) {ITERATE} else {ITERATE1},
              lambda(p, $5),
              $3
            )
          )
        );
      }
    ;

defs:
    def
    | defs def
    ;

def:
    v act2 indent Equal here rhs outdent {
        let l = $1;
        let r = $6;
        let f = head(l);
        if (tag[f]==ID && !isconstructor(f)) {
          /* fnform defn */
          while (tag[l]==AP) {
            r = lambda(tl[l], r);
            l = hd[l];
          }
        }
        r = label($5, r);
        /* to help locate type errors */
        declare(l, r);
        lastname = l;
      }

    | spec {
        let h = reverse(hd[$1]);
        let hr = hd[tl[$1]];
        let t = tl[tl[$1]];
        while (h!=NIL && !SYNERR) {
          specify(hd[h], t, hr);
          h = tl[h];
        }
        $$ = cons(nill, NIL);
      }

    | AbsoluteType here typeforms indent With lspecs outdent {
        // extern word TABSTRS;
        // extern char *dicp, *dicq;
        let x=reverse($6);
        let ids = NIL;
        let tids = NIL;
        while (x!=NIL && !SYNERR) {
          specify(hd[hd[x]], cons(tl[tl[hd[x]]], NIL), hd[tl[hd[x]]]);
          ids = cons(hd[hd[x]], ids);
          x = tl[x];
        }
        /* each id in specs has its id_type set to const(t, NIL) as a way
            of flagging that t is an abstract type */
        x = reverse($3);
        while (x!=NIL && !SYNERR) {
          let shfn;
          decltype(hd[x], abstract_t, undef_t, $2);
          tids = cons(head(hd[x]), tids);
          /* check for presence of showfunction */
          (void)strcpy(dicp, "show");
          (void)strcat(dicp, get_id(hd[tids]));
          dicq = dicp + strlen(dicp) + 1;
          shfn = name();
          if (member(ids, shfn)) {
            t_showfn(hd[tids]) = shfn;
          }
          x = tl[x];
        }
        TABSTRS = cons(cons(tids, ids), TABSTRS);
        $$ = cons(nill, NIL);
      }

    | typeform indent act1 here EqualEqual type act2 outdent {
        let x = redtvars(ap($1, $6));
        decltype(hd[x], synonym_t, tl[x], $4);
        $$ = cons(nill, NIL);
      }

    | typeform indent act1 here Colon2Equal construction act2 outdent {
        let rhs = $6;
        let r_ids = $6;
        let n = 0;
        while (r_ids!=NIL) {
          r_ids = tl[r_ids];
          n += 1;
        }
        while (rhs!=NIL && !SYNERR) {
          let h = hd[rhs];
          let t = $1;
          let stricts = NIL;
          let i = 0;
          while(tag[h]==AP) {
            if (tag[tl[h]]==AP && hd[tl[h]]==strict_t) {
              stricts = cons(i, stricts);
              tl[h] = tl[tl[h]];
            }
            t = ap2(arrow_t, tl[h], t);
            h = hd[h];
            i += 1;
          }
          if(tag[h]==ID){
            n -= 1;
            declconstr(h, n, t);
          } else {
            /* warning - type not yet in reduced form */

            stricts = NIL;
            if(echoing){
              putchar('\n');
            }
            printf("syntax error: illegal construct \"");
            out_type(hd[rhs]);
            printf("\" on right of ::=\n");
            acterror();
          } /* can this still happen? check later */
          if (stricts!=NIL) { /* ! operators were present */
            word k = id_val(h);
            while (stricts!=NIL) {
              k = ap2(MKSTRICT, i - hd[stricts], k);
              stricts = tl[stricts];
            }
            id_val(h) = k; /* overwrite id_val of original constructor */
          }
          r_ids = cons(h, r_ids);
          rhs = tl[rhs];
        }
        if (!SYNERR) {
          decltype($1, algebraic_t, r_ids, $4);
        }
        $$ = cons(nill, NIL);
      }

    | indent setexp Export parts outdent {
        inexplist = 0;
        if (exports!=NIL) {
          errs = $2;
          syntax("multiple %export statements are illegal\n");
        } else {
          if ($4==NIL && exportfiles==NIL && embargoes!=NIL) {
            exportfiles = cons(Plus, NIL);
          }
          exports = cons($2, $4);  /* cons(hereinfo, identifiers) */
        }
        $$ = cons(nill, NIL);
      }

    | Free here OpenBrace specs CloseBrace {
        if (freeids!=NIL) {
          errs=$2;
          syntax("multiple %free statements are illegal\n");
        } else {
          let x = reverse($4);
          while (x!=NIL&&!SYNERR) {
            specify(hd[hd[x]], tl[tl[hd[x]]], hd[tl[hd[x]]]);
            freeids = cons(head(hd[hd[x]]), freeids);
            if (tl[tl[hd[x]]]==type_t) {
              t_class(hd[freeids]) = free_t;
            }
            else {
              id_val(hd[freeids]) = FREE; /* conventional value */
            }
            x = tl[x];
          }
          fil_share(hd[files]) = 0; /* parameterised scripts unshareable */
          freeids = alfasort(freeids);
          for(x=freeids; x!=NIL; x=tl[x]){
            /* each element of freeids is of the form
              cons(id, cons(original_name, type)) */
            hd[x] = cons(
              hd[x],
              cons(
                datapair(get_id(hd[x]), 0),
                id_type(hd[x])
              )
            );
          }
        }
        $$ = cons(nill, NIL);
      }

    | Include bindings modifiers outdent { /* fiddle - 'indent' done by yylex() on reading fileid */
        // extern char *dicp;
        // extern word CLASHES, BAD_DUMP;
        /* $1 contains file+hereinfo */
        includees = cons(cons($1, cons($3, $2)), includees);
        $$ = cons(nill, NIL);
      }

    | here BNF { startbnf(); inbnf = 1; } names outdent productions EndIR {
        /* fiddle - `indent' done by yylex() while processing directive */
        let lhs = NIL;
        let p=$6;
        let subjects;
        let body;
        let startswith = NIL;
        let leftrecs = NIL;
        ihlist = 0;
        inbnf = 0;
        nonterminals = UNION(nonterminals, $4);
        for(;p!=NIL;p=tl[p]){
          if (dval(hd[p])==UNDEF) {
            nonterminals = add1(dlhs(hd[p]), nonterminals);
          }
          else{
            lhs = add1(dlhs(hd[p]), lhs);
          }
        }
        nonterminals = setdiff(nonterminals, lhs);
        if (nonterminals!=NIL) {
          errs = $1;
          member($4, hd[nonterminals]); /*||findnt(hd[nonterminals])*/
          printf("%sfatal error in grammar, ", echoing?"\n":"");
          printf(
            "undefined nonterminal%s: ",
            if tl[nonterminals]==NIL {""} else {"s"}
          );
          printlist("", nonterminals);
          acterror();
        } else { /* compute list of nonterminals admitting empty prodn */
          eprodnts = NIL;
          L:
          for(p = $6; p!=NIL; p = tl[p]) {
            if(!member(eprodnts, dlhs(hd[p])) && eprod(dval(hd[p]))) {
              eprodnts = cons(dlhs(hd[p]), eprodnts);
              goto L;
            }
          }
          /*
            now compute startswith reln between nonterminals
            (performing binomial transformation en route)
            and use to detect unremoved left recursion
          */
          for(p=$6;p!=NIL;p=tl[p]) {
            lhs = starts(dval(hd[p]));
            if (member(lhs, dlhs(hd[p]))) {
              binom(dval(hd[p]), dlhs(hd[p]));
              startswith = cons(
                cons(
                  dlhs(hd[p]),
                  starts(dval(hd[p]))
                ),
                startswith
              );
            } else {
              startswith = cons(cons(dlhs(hd[p]), lhs), startswith);
            }
          }
          startswith = tclos(sortrel(startswith));
          for(; startswith!=NIL; startswith = tl[startswith]) {
            if (member(tl[hd[startswith]], hd[hd[startswith]])) {
              leftrecs = add1(hd[hd[startswith]], leftrecs);
            }
          }
          if(leftrecs!=NIL){
            errs = getloc(hd[leftrecs], $6);
            printf(
              "%sfatal error in grammar, ",
              if echoing {"\n"} else {""}
            );
            printlist("irremovable left recursion: ", leftrecs);
            acterror();
          }
          if($4==NIL) { /* implied start symbol */
            $4 = cons(dlhs(hd[lastlink($6)]), NIL);
          }
          fnts = 1; /* fnts is flag indicating %bnf in use */
          if (tl[$4]==NIL) { /* only one start symbol */
            subjects = getfname(hd[$4]);
            body = ap2(G_CLOSE, str_conv(get_id(hd[$4])), hd[$4]);
          } else {
            body = Void;
            subjects = Void;
            while ($4!=NIL) {
              subjects = pair(getfname(hd[$4]), subjects);
              body = pair(
                ap2(G_CLOSE, str_conv(get_id(hd[$4])), hd[$4]),
                body
              );
              $4 = tl[$4];
            }
          }
          declare(subjects, label($1, block($6, body, 0)));
        }
      }
    ;

setexp:
    here {
        /* hack to fix lex analyser */
        $$ = $1;
        inexplist = 1;
      }
    ;

bindings:
    /* empty */ { $$ = NIL; }
    | OpenBrace bindingseq CloseBrace { $$ = $2; }
    ;

bindingseq:
    bindingseq binding { $$ = cons($2, $1); }
    | binding { $$ = cons($1, NIL); }
    ;

binding:
    Name indent Equal exp outdent { $$ = cons($1, $4); }
    | typeform indent act1 EqualEqual type act2 outdent {
        let x = redtvars(ap($1, $5));
        let arity = 0;
        let h = hd[x];
        while (tag[h]==AP) {
          arity += 1;
          h = hd[h];
        }
        $$ = ap(h, make_typ(arity, 0, synonym_t, tl[x]));
      }
    ;

modifiers:
    /* empty */ { $$ = NIL; }
    | negmods {
        let a;
        let b;
        let c = 0;
        for(a = $1; a!=NIL; a = tl[a]) {
          for(b = tl[a]; b!=NIL; b = tl[b]) {
            if(hd[hd[a]]==hd[hd[b]]) {
              c = hd[hd[a]];
            }
            if (tl[hd[a]]==tl[hd[b]]) {
              c = tl[hd[a]];
            }
            if(c){
              break;
            }
          }
        }
        if(c) {
          printf(
            "%ssyntax error: conflicting aliases (\"%s\")\n",
            if echoing {"\n"} else {""},
            get_id(c)
          );
          acterror();
        }
      }
    ;

negmods:
    negmods negmod { $$ = cons($2, $1); }
    | negmod { $$ = cons($1, NIL); }
    ;

negmod:
    Name Divide Name { $$ = cons($1, $3); }
    | ConstructorName Divide ConstructorName { $$ = cons($1, $3); }
    | Minus Name { $$ = cons(make_pn(UNDEF), $2); }
    /*| Minus ConstructorName  no - cannot suppress constructors selectively */
    ;

here:
    /* empty */ {
        // extern word line_no;
        lasth = fileinfo(get_fil(current_file), line_no);
        $$ = lasth;
        /* (script, line_no) for diagnostics */
      }
    ;

act1:
    /* empty */ { tvarscope = 1; };

act2:
    /* empty */ {
        tvarscope = 0;
        idsused = NIL;
      }
    ;

ldefs:
    ldef {
        $$ = cons($1, NIL);
        dval($1) = tries(dlhs($1), cons(dval($1), NIL));
        if (!SYNERR && get_ids(dlhs($1))==NIL) {
          errs = hd[hd[tl[dval($1)]]];
          syntax("illegal lhs for local definition\n");
        }
      }
    | ldefs ldef {
        if(dlhs($2)==dlhs(hd[$1]) /*&&dval(hd[$1])!=UNDEF*/) {
          $$ = $1;
          if (!fallible(hd[tl[dval(hd[$1])]])) {
            errs = hd[dval($2)];
            printf(
              "%ssyntax error: unreachable case in defn of \"%s\"\n",
              if echoing {"\n"} else {""},
              get_id(dlhs($2))
            );
            acterror();
          }
          tl[dval(hd[$1])] = cons(dval($2), tl[dval(hd[$1])]);
        } else if (!SYNERR) {
          let ns = get_ids(dlhs($2));
          let hr = hd[dval($2)];
          if(ns==NIL){
            errs = hr;
            syntax("illegal lhs for local definition\n");
          }
          $$ = cons($2, $1);
          dval($2) = tries(dlhs($2), cons(dval($2), NIL));
          while(ns!=NIL&&!SYNERR) { /* local nameclash check */
            nclashcheck(hd[ns], $1, hr);
            ns = tl[ns];
          } /* potentially quadratic - fix later */
        }
      }
   ;

ldef:
    spec {
        errs = hd[tl[$1]];
        syntax("`::' encountered in local defs\n");
        $$ = cons(nill, NIL);
      }
    | typeform here EqualEqual {
        errs=$2;
        syntax("`==' encountered in local defs\n");
        $$ = cons(nill, NIL);
      }
    | typeform here Colon2Equal {
        errs=$2;
        syntax("`::=' encountered in local defs\n");
        $$ = cons(nill, NIL);
      }
    | v act2 indent Equal here rhs outdent {
        let l = $1;
        let r = $6;
        word f = head(l);
        if(tag[f]==ID&&!isconstructor(f)) { /* fnform defn */
          while(tag[l]==AP){
            r = lambda(tl[l], r);
            l = hd[l];
          }
        }
        r = label($5, r); /* to help locate type errors */
        $$ = defn(l, undef_t, r);
      }
    ;

vlist:
    v { $$ = cons($1, NIL); }
    | vlist Comma v /* left recursive so as not to eat YACC stack */ {
        /* reverse order, NB */
        $$ = cons($3, $1);
      }
    ;

v:
    v1
    | v1 Colon v { $$ = cons($1, $3); }
    ;

v1:
    v1 Plus Constant  /* n+k pattern */ {
        if (!isnat($3)){
          syntax("inappropriate use of \"+\" in pattern\n");
        }
        $$ = ap2(Plus, $3, $1);
      }
    | Minus Constant {
        /* if(tag[$2]==DOUBLE)
              $$ = cons(Constant, sto_dbl(-get_dbl($2))); else */
        if (tag[$2]==INT) {
          $$ = cons(Constant, bignegate($2));
        } else {
          syntax("inappropriate use of \"-\" in pattern\n");
        }
      }
    | v2 InfixName v1 { $$ = ap2($2, $1, $3); }
    | v2 InfixCName v1 { $$ = ap2($2, $1, $3); }
    | v2;

v2:
    v3
    | v2 v3 {
        $$ = ap(
          if hd[$1]==Constant && tag[tl[$1]]==ID {
            tl[$1]
          } else {
            $1
          },
          $2
        );
        /* repeated name apparatus may have wrapped Constant around leading id
           - not wanted */
      }
    ;

v3:
    Name {
        if (sreds && member(gvars, $1)){
          syntax("illegal use of $num symbol\n");
        }
        /* cannot use grammar variable in a binding position */
        if (memb(idsused, $1)) {
          $$ = cons(Constant, $1);
        } /* picks up repeated names in a template */
        else {
          idsused = cons($1, idsused);
        }
      }
    | ConstructorName
    | Constant {
        if (tag[$1]==DOUBLE) {
          syntax("use of floating point literal in pattern\n");
        }
        $$ = cons(Constant, $1);
      }
    | OpenBracket CloseBracket { $$ = nill; }
    | OpenBracket vlist CloseBracket {
        let x = $2;
        let y = nill;
        while(x!=NIL) {
          y = cons(hd[x], y);
          x = tl[x];
        }
        $$ = y;
      }
    | OpenParenthesis CloseParenthesis { $$ = Void; }
    | OpenParenthesis v CloseParenthesis { $$ = $2; }
    | OpenParenthesis v Comma vlist CloseParenthesis {
        /* representation of the tuple (a1, ..., an) is
             tcons(a1, tcons(a2, ...pair(a(n-1), an))) */
        if(tl[$4]==NIL) {
          $$=pair($2, hd[$4]);
        } else {
          $$ = pair(hd[tl[$4]], hd[$4]);
          $4 = tl[tl[$4]];
          while ($4!=NIL) {
            $$=tcons(hd[$4], $$);
            $4=tl[$4];
          }
          $$ = tcons($2, $$);
        }

      }
    ;

type:
    type1
    | type Arrow type { $$ = ap2(arrow_t, $1, $3); }
    ;

type1:
    type2 InfixName type1 { $$ = ap2($2, $1, $3); }
    | type2
    ;

type2:
    /* type2 argtype  /* too permissive - fix later */
        /* = { $$ = ap($1, $2); }| */
    tap
    | argtype
    ;

tap:
    Name argtype { $$ = ap($1, $2); }
    | tap argtype { $$ = ap($1, $2); }
    ;

argtype:
    Name { $$ = transtypeid($1); }
           /* necessary while prelude not meta_tchecked (for prelude) */
    | typevar {
        if (tvarscope && !memb(idsused, $1)){
          printf(
            "%ssyntax error: unbound type variable ",
            if echoing {"\n"} else {""}
          );
          out_type($1);
          putchar('\n');
          acterror();
        }
        $$ = $1;
      }
    | OpenParenthesis typelist CloseParenthesis { $$ = $2; }
    | OpenBracket type CloseBracket  /* at release one was `typelist' */ { $$ = ap(list_t, $2); }
    | OpenBracket type Comma typel CloseBracket {
        syntax(
          "tuple-type with missing parentheses (obsolete syntax)\n"
        );
      }
    ;

typelist:
    /* empty */ { $$ = void_t; }  /* voidtype */
    | type
    | type Comma typel {
        let x = $3;
        let y = void_t;
        while (x!=NIL) {
          y = ap2(comma_t, hd[x], y);
          x = tl[x];
        }
        $$ = ap2(comma_t, $1, y);
      }
    ;

typel:
    type { $$ = cons($1, NIL); }
    | typel Comma type /* left recursive so as not to eat YACC stack */ { $$ = cons($3, $1); }
    ;

parts: /* returned in reverse order */
    parts Name { $$ = add1($2, $1); }
    | parts Minus Name {
        $$ = $1;
        embargoes = add1($3, embargoes);
      }
    | parts PathName { $$ = $1; } /*the pathnames are placed on exportfiles in yylex*/
    | parts Plus {
        $$ = $1;
        exportfiles = cons(Plus, exportfiles);
      }
    | Name { $$ = add1($1, NIL); }
    | Minus Name {
        $$ = NIL;
        embargoes = add1($2, embargoes);
      }
    | PathName { $$ = NIL; }
    | Plus {
        $$ = NIL;
        exportfiles=cons(Plus, exportfiles);
      }
    ;

specs:  /* returns a list of cons(id, cons(here, type))
           in reverse order of appearance */
    specs spec {
        let x = $1;
        let h = hd[$2];
        let t = tl[$2];
        while (h!=NIL) {
          x = cons(cons(hd[h], t), x);
          h = tl[h];
        }
        $$ = x;
      }
    | spec {
        let x = NIL;
        let h = hd[$1];
        let t = tl[$1];
        while (h!=NIL) {
          x = cons(cons(hd[h], t), x);
          h = tl[h];
        }
        $$ = x;
      }
    ;

spec:
    typeforms indent here ColonColon ttype outdent {
        $$ = cons($1, cons($3, $5));
      } /* hack: `typeforms' includes `namelist' */
    ;

lspecs:  /* returns a list of cons(id, cons(here, type))
           in reverse order of appearance */
    lspecs lspec {
        let x = $1;
        let h = hd[$2];
        let t = tl[$2];
        while(h!=NIL) {
          x = cons(cons(hd[h], t), x);
          h = tl[h];
        }
        $$ = x;
      }
    | lspec {
        let x = NIL;
        let h = hd[$1];
        let t = tl[$1];
        while(h!=NIL) {
          x = cons(cons(hd[h], t), x);
          h = tl[h];
        }
        $$ = x;
      }
    ;

lspec:
    namelist indent here {inbnf=0;} ColonColon type outdent { $$ = cons($1, cons($3, $6)); };

namelist:
    Name Comma namelist { $$ = cons($1, $3); }
    | Name { $$ = cons($1, NIL); }
    ;

typeforms:
    typeforms Comma typeform act2 { $$ = cons($3, $1); }
    | typeform act2 { $$ = cons($1, NIL); }
    ;

typeform:
    ConstructorName typevars { syntax("upper case identifier out of context\n"); }
    | Name typevars   /* warning if typevar is repeated */ {
        $$ = $1;
        idsused = $2;
        while($2!=NIL) {
          $$ = ap($$, hd[$2]);
          $2 = tl[$2];
        }
      }
    | typevar InfixName typevar {
        if(eqtvar($1, $3)){
          syntax("repeated type variable in typeform\n");
        }
        idsused = cons($1, cons($3, NIL));
        $$ = ap2($2, $1, $3);
      }
    | typevar InfixCName typevar {
        syntax("upper case identifier cannot be used as typename\n");
      }
    ;

ttype:
    type
    | Type { $$ = type_t; }
    ;

typevar:
    Times { $$ = mktvar(1); }
    | TypeVar
    ;

typevars:
    /* empty */ { $$ = NIL; }
    | typevar typevars {
        if(memb($2, $1)) {
              syntax("repeated type variable on lhs of type def\n");
        }
        $$ = cons($1, $2);
      }
    ;

construction:
    constructs { /* keeps track of sui-generis constructors */
        // extern word SGC;
        if ( tl[$1]==NIL && tag[hd[$1]]!=ID ) {
                        /* 2nd conjunct excludes singularity types */
          SGC = cons(head(hd[$1]), SGC);
        }
      }
    ;

constructs:
    construct { $$ = cons($1, NIL); }
    | constructs Pipe construct { $$ = cons($3, $1); }
    ;

construct:
    field here InfixCName field {
        $$ = ap2($3, $1, $4);
        id_who($3) = $2;
      }
    | construct1
    ;

construct1:
    OpenParenthesis construct CloseParenthesis { $$ = $2; }
    | construct1 field1 { $$ = ap($1, $2); }
    | here ConstructorName {
        $$ = $2;
        id_who($2) = $1;
      }
    ;

field:
    type
    | argtype Bang { $$ = ap(strict_t, $1); }
    ;

field1:
    argtype Bang { $$ = ap(strict_t, $1); }
    | argtype
    ;

names:          /* used twice - for bnf list, and for inherited attr list */
    /* empty */ { $$ = NIL; }
    | names Name {
        if(member($1, $2)){
          printf(
            "%ssyntax error: repeated identifier \"%s\" in %s list\n",
            if echoing {"\n"} else {""},
            get_id($2),
            if inbnf {"bnf"} else {"attribute"}
          );
          acterror();
        }
        $$ = if inbnf {
          add1($2, $1)
        } else {
          cons($2, $1)
        };
      }
    ;

productions:
    lspec {
        let h = reverse(hd[$1]);
        let hr = hd[tl[$1]];
        let t = tl[tl[$1]];
        inbnf = 1;
        $$ = NIL;
        while (h!=NIL && !SYNERR) {
          ntspecmap = cons(cons(hd[h], hr), ntspecmap);
          $$ = add_prod(defn(hd[h], t, UNDEF), $$, hr);
          h = tl[h];
        }
      }
    | production { $$ = cons($1, NIL); }
    | productions lspec {
        let h = reverse(hd[$2]);
        let hr = hd[tl[$2]];
        let t = tl[tl[$2]];
        inbnf = 1;
        $$=$1;
        while(h!=NIL&&!SYNERR){
          ntspecmap = cons(cons(hd[h], hr), ntspecmap);
          $$ = add_prod(defn(hd[h], t, UNDEF), $$, hr);
          h = tl[h];
        }
      }
    | productions production { $$ = add_prod($2, $1, hd[dval($2)]); }
    ;

production:
    Name params Colon indent grhs outdent
      /* found by experiment that indent must follow Colon here */ {
        $$ = defn($1, undef_t, $5);
      }
    ;

params:   /* places inherited attributes, if any, on ihlist */
    /* empty */ { ihlist=0; }
    | { inbnf=0; } OpenParenthesis names CloseParenthesis {
        inbnf = 1;
        if ($3==NIL) {
          syntax("unexpected token CloseParenthesis\n");
        }
        ihlist=$3;
      }
    ;

grhs:
    here phrase { $$ = label($1, $2); }
    ;

phrase:
    error_term { $$ = ap2(G_ERROR, G_ZERO, $1); }
    | phrase1 {
        $$ = hd[$1];
        $1 = tl[$1];
        while ($1!=NIL) {
          $$ = label(hd[$1], $$);
          $1 = tl[$1];
          $$ = ap2(G_ALT, hd[$1], $$);
          $1 = tl[$1];
        }
      }
    | phrase1 Pipe error_term {
        $$=hd[$1];
        $1=tl[$1];
        while($1!=NIL){
          $$ = label(hd[$1], $$);
          $1 = tl[$1];
          $$ = ap2(G_ALT, hd[$1], $$);
          $1 = tl[$1];
        }
        $$ = ap2(G_ERROR, $$, $3);
      }
      /* we right rotate G_ALT's to facilitate left factoring (see trans) */
    ;

phrase1:
    term { $$=cons($1, NIL); }
    | phrase1 Pipe here term { $$ = cons($4, cons($3, $1)); }
    ;

term:
    count_factors { }
    | count_factors {inbnf=2;} indent Equal here rhs outdent { }
    ;

error_term:
    ErrorSymbol { }
    | ErrorSymbol { inbnf=2, sreds=2; } indent Equal here rhs outdent { }
    ;

count_factors:
    EmptySymbol {
        sreds = 0;
        $$ = NIL;
      }
    | EmptySymbol factors {
        syntax("unexpected token after empty\n");
        sreds = 0;
        $$ = NIL;
	    }
    | { obrct=0; } factors { }
    ;

factors:
    factor { $$ = cons($1, NIL); }
    | factors factor {
        if (hd[$1]==G_END) {
          syntax("unexpected token after end\n");
        }
        $$ = cons($2, $1);
      }
    ;

factor:
    unit
    | OpenBrace unit CloseBrace {
        $$ = ap(outdent_fn, ap2(indent_fn, getcol_fn(), $2));
      }
    | OpenBrace unit {
        obrct += 1;
        $$ = ap2(indent_fn, getcol_fn(), $2);
      }
    | unit CloseBrace {
        obrct -= 1;
	      if obrct < 0 {
	        syntax("unmatched `}' in grammar rule\n");
	      }
        $$ = ap(outdent_fn, $1);
      }
    ;

unit:
    symbol
    | symbol Times { $$ = ap(G_STAR, $1); }
    | symbol Plus {
        $$ = ap2(G_SEQ, $1, ap2(G_SEQ, ap(G_STAR, $1), ap(G_RULE, ap(C, P))));
      }
    | symbol QuestionMark { $$ = ap(G_OPT, $1); }
    ;

symbol:
    Name {
        //extern word NEW;
        nonterminals = newadd1($1, nonterminals);
        if(NEW){
          ntmap = cons(cons($1, lasth), ntmap);
        }
      }
    | EndSymbol { $$ = G_END; }
    | Constant {
        if(!isstring($1)) {
          printf(
            "%ssyntax error: illegal terminal ",
            if echoing {"\n" }else {""}
          );
          out(stdout, $1);
          printf(" (should be string-const)\n");
          acterror();
        }
        $$ = ap(G_SYMB, $1);
      }
    | Caret { $$=G_STATE; }
    | {inbnf=0;} OpenBracket exp {inbnf=1;} CloseBracket { $$ = ap(G_SUCHTHAT, $3); }
    | Minus { $$ = G_ANY; }
    ;

%%
/*  end of Miranda rules  */
