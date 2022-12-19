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

use crate::data::values;
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
    | Bang { $$ = self.heap.apply(Combinator::C.into(), Combinator::Subscript.into()); }
    | InfixName
    | InfixCName
    ;

relop:
    Greater { $$ = GR; }
    | GreaterEqual { $$ = GRE; }
    | eqop { $$ = EQ; }
    | NotEqual { $$ = NEQ; }
    | LessEqual { $$ = self.heap.apply(C, GRE); }
    | Less { $$ = self.heap.apply(C, GR); }
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
    exp Comma if exp { $$ = self.heap.cons(self.heap.apply2(COND, $4, $1), NIL); }
    | exp Comma Otherwise {
    	let ow = self.heap.apply(Otherwise, $1);
    	$$ = self.heap.cons(ow, NIL);
    }
    | cases reindent ElseEqual alt {
        $$ = self.heap.cons($4, $1);
        if(self.hd_hd($1) == Otherwise){
          syntax("\"otherwise\" must be last case\n");
        }
      }
    ;

alt:
    here exp {
        errs=$1;
        syntax("obsolete syntax, \", otherwise\" missing\n");
        $$ = self.heap.apply(Otherwise, label($1, $2));
      }
    | here exp Comma if exp { $$ = label($1, self.heap.apply2(COND, $5, $2)); }
    | here exp Comma Otherwise { $$ = self.heap.apply(Otherwise, label($1, $2)); }
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
    exp { $$ = self.heap.cons($1, NIL); }
    | liste Comma exp  /* left recursive so as not to eat YACC stack */
        { $$ = self.heap.cons($3, $1); }
    ;

e1:
    Tilde e1 %prec Equal { $$ = self.heap.apply(NOT, $2); }
    | e1 PlusPlus e1 { $$ = self.heap.apply2(Append, $1, $3); }
    | e1 Colon e1 { $$ = self.heap.cons($1, $3); }
    | e1 MinusMinus e1 { $$ = self.heap.apply2(listdiff_fn, $1, $3);  }
    | e1 Vel e1 { $$ = self.heap.apply2(OR, $1, $3); }
    | e1 Ampersand e1 { $$ = self.heap.apply2(And, $1, $3); }
    | reln
    | e2
    ;

es1:                     /* e1 or presection */
    Tilde e1 %prec Equal { $$ = self.heap.apply(NOT, $2); }
    | e1 PlusPlus e1 { $$ = self.heap.apply2(Append, $1, $3); }
    | e1 PlusPlus { $$ = self.heap.apply(Append, $1); }
    | e1 Colon e1 { $$ = self.heap.cons($1, $3); }
    | e1 Colon { $$ = self.heap.apply(P, $1); }
    | e1 MinusMinus e1 { $$ = self.heap.apply2(listdiff_fn, $1, $3);  }
    | e1 MinusMinus { $$ = self.heap.apply(listdiff_fn, $1);  }
    | e1 Vel e1 { $$ = self.heap.apply2(OR, $1, $3); }
    | e1 Vel { $$ = self.heap.apply(OR, $1); }
    | e1 Ampersand e1 { $$ = self.heap.apply2(And, $1, $3); }
    | e1 Ampersand { $$ = self.heap.apply(And, $1); }
    | relsn
    | es2
    ;

e2:
    Minus e2 %prec Minus { $$ = self.heap.apply(NEG, $2); }
    | Hash e2 %prec Dot { $$ = self.heap.apply(LENGTH, $2);  }
    | e2 Plus e2 { $$ = self.heap.apply2(Plus, $1, $3); }
    | e2 Minus e2 { $$ = self.heap.apply2(Minus, $1, $3); }
    | e2 Times e2 { $$ = self.heap.apply2(Times, $1, $3); }
    | e2 Divide e2 { $$ = self.heap.apply2(Divide, $1, $3); }
    | e2 IntegerDivide e2 { $$ = self.heap.apply2(IntegerDivide, $1, $3); }
    | e2 Remainder e2 { $$ = self.heap.apply2(Remainder, $1, $3); }
    | e2 Caret e2 { $$ = self.heap.apply2(Power, $1, $3); }
    | e2 Dot e2 { $$ = self.heap.apply2(B, $1, $3);  }
    | e2 Bang e2 { $$ = self.heap.apply2(SUBSCRIPT, $3, $1); }
    | e3
    ;

es2:               /* e2 or presection */
    Minus e2 %prec Minus { $$ = self.heap.apply(NEG, $2); }
    | Hash e2 %prec Dot { $$ = self.heap.apply(LENGTH, $2);  }
    | e2 Plus e2 { $$ = self.heap.apply2(Plus, $1, $3); }
    | e2 Plus { $$ = self.heap.apply(Plus, $1); }
    | e2 Minus e2 { $$ = self.heap.apply2(Minus, $1, $3); }
    | e2 Minus { $$ = self.heap.apply(Minus, $1); }
    | e2 Times e2 { $$ = self.heap.apply2(Times, $1, $3); }
    | e2 Times { $$ = self.heap.apply(Times, $1); }
    | e2 Divide e2 { $$ = self.heap.apply2(Divide, $1, $3); }
    | e2 Divide { $$ = self.heap.apply(Divide, $1); }
    | e2 IntegerDivide e2 { $$ = self.heap.apply2(IntegerDivide, $1, $3); }
    | e2 IntegerDivide { $$ = self.heap.apply(IntegerDivide, $1); }
    | e2 Remainder e2 { $$ = self.heap.apply2(Remainder, $1, $3); }
    | e2 Remainder { $$ = self.heap.apply(Remainder, $1); }
    | e2 Caret e2 { $$ = self.heap.apply2(Power, $1, $3); }
    | e2 Caret { $$ = self.heap.apply(Power, $1); }
    | e2 Dot e2 { $$ = self.heap.apply2(B, $1, $3);  }
    | e2 Dot { $$ = self.heap.apply(B, $1);  }
    | e2 Bang e2 { $$ = self.heap.apply2(SUBSCRIPT, $3, $1); }
    | e2 Bang { $$ = self.heap.apply2(C, SUBSCRIPT, $1); }
    | es3
    ;

e3:
    comb InfixName e3 { $$ = self.heap.apply2($2, $1, $3); }
    | comb InfixCName e3 { $$ = self.heap.apply2($2, $1, $3); }
    | comb
    ;

es3:                     /* e3 or presection */
    comb InfixName e3 { $$ = self.heap.apply2($2, $1, $3); }
    | comb InfixName { $$ = self.heap.apply($2, $1); }
    | comb InfixCName e3 { $$ = self.heap.apply2($2, $1, $3); }
    | comb InfixCName { $$ = self.heap.apply($2, $1); }
    | comb
    ;

comb:
    comb arg { $$ = self.heap.apply($1, $2); }
    | arg
    ;

reln:
    e2 relop e2 { $$ = self.heap.apply2($2, $1, $3); }
    | reln relop e2 {
        /* EFFICIENCY PROBLEM - subject gets re-evaluated (and
            retypechecked) - fix later */
            let hd = self.heap[$1].head;
        let subject = if self.heap[hd].head == And {
          self.tl_tl($1)
        } else {
          self.heap[$1].tail
        };
        let rhs = self.heap.apply2($2, subject, $3);
        $$ = self.heap.apply2(And, $1, rhs);
      }
    ;

relsn:                     /* reln or presection */
    e2 relop e2 { $$ = self.heap.apply2($2, $1, $3); }
    | e2 relop { $$ = self.heap.apply($2, $1); }
    | reln relop e2 {
        /* EFFICIENCY PROBLEM - subject gets re-evaluated (and
                    retypechecked) - fix later */

        let subject = if self.hd_hd($1) == And {
          self.tl_tl($1)
        } else {
          self.heap[$1].tail
        };
        let rhs = self.heap.apply2($2, subject, $3);
        $$ = self.heap.apply2(And, $1, rhs);
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
                      let mut echoed = 0;
                      while lexstates!=NIL {
                        if !echoed {
                          if echoing {
                            println!("");
                          }
                          echoed = 1;
                        }
                        let mut lexstate_hd = self.heap[lexstates].head;
                        let mut lexstate_hd_tl  = self.heap[lexstate_hd].tail;
                        if !(lexstate_hd_tl & 1) {
                          println!(
                            "warning: lex state {} is never entered",
                            get_id(self.heap[lexstate_hd].head)
                          );
                        } else {
                          // Todo: What are these magic values?
                          if !(lexstate_hd_tl & 2) {
                            println!(
                              "warning: lex state {} has no associated rules",
                              get_id(self.heap[lexstate_hd].head)
                            );
                          }
                        }

                        lexstates = self.heap[lexstates].tail;
                      }
                    }
                    if $3 == NIL {
                    syntax("%lex with no rules\n");
                  } else {
                    self.heap[$3].tag = LEXER;
                  }

                    /*
                       result is lex-list, in reverse order, of items of the form
                         self.heap.cons(scstuff, self.heap.cons(matcher, rhs))
                       where scstuff is of the form
                         self.heap.cons(0-or-list-of-startconditions, 1+newstartcondition)
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
    | OpenBracket exp CloseBracket { $$ = self.heap.cons($2, NIL); }
    | OpenBracket exp Comma exp CloseBracket {
        let rhs = self.heap.cons($4, NIL);
    	$$ = self.heap.cons($2, rhs);
    }
    | OpenBracket exp Comma exp Comma liste CloseBracket {
       let rhs = self.heap.cons($4, reverse($6));
    	$$ = self.heap.cons($2, rhs);
    }
    | OpenBracket exp DotDot exp CloseBracket { $$ = self.heap.apply3(STEPUNTIL, big_one, $4, $2); }
    | OpenBracket exp DotDot CloseBracket { $$ = self.heap.apply2(STEP, big_one, $2); }
    | OpenBracket exp Comma exp DotDot exp CloseBracket {
        let minus_expr = self.heap.apply2(Minus, $4, $2);
        $$ = self.heap.apply3(STEPUNTIL, minus_expr, $6, $2);
    }
    | OpenBracket exp Comma exp DotDot CloseBracket {
        let minus_expr= self.heap.apply2(Minus, $4, $2);
        $$ = self.heap.apply2(STEP, minus_expr, $2);
     }
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
        let hd = self.heap[$2].head;
        $$ = if self.heap[$2].tag == AP && hd == C {
          self.heap.apply(hd, $3)
        } else {
          self.heap.apply2(C, $2, $3)
        };
      }
    | OpenParenthesis CloseParenthesis { $$ = Void; }  /* the void tuple */
    | OpenParenthesis exp Comma liste CloseParenthesis {
        if self.heap[$4].tail==NIL {
          $$ = pair($2, self.heap[$4].head);
        } else {
          let tl = self.heap[$4].tail;
          $$ = pair(self.heap[tl].head, self.heap[$4].head);
          $4 = self.heap[tl].tail;
          while ($4!=NIL) {
            let hd = self.heap[$4].head;
            $$ = self.heap.cons(hd, $$);
            $4 = self.heap[$4].tail;
          }
          $$ = self.heap.cons($2, $$);
        }
        /* representation of the tuple (a1, ..., an) is
            self.heap.cons(a1, self.heap.cons(a2, ...pair(a(n-1), an))) */
      }
    ;

lexrules:
    lexrules lstart here re indent { if(!SYNERR){ inlex=2; } }

    Arrow exp lpostfix { if(!SYNERR){ inlex=1; } } outdent {
        if $9<0 && e_re($4) {
          errs = $3;
          syntax("illegal lex rule - lhs matches empty\n");
        }
        let cons_a = self.heap.cons($2, 1+$9);
        let cons_b = self.heap.cons($4, label($3, $8));
        let cons = self.heap.cons(cons_a, cons_a)
        $$ = self.heap.cons(cons, $1);
      }
    | lexdefs { $$ = NIL; }
    ;

lstart:
    /* empty */ { $$ = 0; }
    | Less cnames Greater {
        let mut ns = NIL;
	while $2 != NIL {
	  let mut x = &lexstates;
	  let mut i = 1;

	  loop {
	      if x != NIL && self.hd_hd(x) != self.heap[$2].head {
		  break;
	      }
	      i += 1;
	      x = &self.heap[*x].tail;
	  }

	  if *x == NIL {
	      let hd = self.heap[$2].head;
	      let cons = self.heap.cons(hd, 2);
	      *x = self.heap.cons(cons, NIL);
	  } else {
	      self.hd_tl(*x) |= 2;
	  }
	  ns = add1(i, ns);
	  $2 = self.heap[$2].tail
	}
	$$ = ns;
      }
    ;

cnames:
    ConstructorName { $$=self.heap.cons($1, NIL); }
    | cnames ConstructorName {
        if member($1, $2) {
          println!(
            "{}syntax error: repeated name \"{}\" in start conditions\n",
            if echoing {"\n"} else {""},
            get_id( $2)
          );
          acterror();
        }
        $$ = self.heap.cons($2, $1);
      }
    ;

lpostfix:
        /* empty */ { $$ = -1; }
        | Begin ConstructorName
        {
          let mut x = &lexstates;
	      let mut i = 1;
	      loop
	      {
		if *x == NIL { break; }
		let hd = self.heap[*x].head;
		if self.heap[hd].head == $2 { break; }
		i += 1;
		x = &tl[*x];
	      }
	      if *x == NIL {
		  let cons = self.heap.cons($2, 1);
		  *x = self.heap.cons(cons, NIL);
	      } else {
		let hd = self.heap[*x].head;
		self.heap[hd].tail |= 1;
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
    lexdefs LexDef indent Equal re outdent {
    		let cons = self.heap.cons($2, $5);
	    	lexdefs = self.heap.cons(cons, lexdefs);
	    }
    | /* empty */ { lexdefs = NIL; }
    ;

re:   /* regular expression */
    re1 Pipe re { $$ = self.heap.apply2(LEX_OR, $1, $3); }
    | re1
    ;

re1:
    lterm Divide lterm { $$ = self.heap.apply2(LEX_RCONTEXT, $1, $3); }
    | lterm Divide { $$ = self.heap.apply2(LEX_RCONTEXT, $1, 0); }
    | lterm
    ;

lterm:
    lfac lterm { $$ = self.heap.apply2(LEX_SEQ, $1, $2); }
    | lfac
    ;

lfac:
    lunit Times {
        if (e_re($1)) {
          syntax("illegal regular expression - arg of * matches empty\n");
        }
        $$ = self.heap.apply(LEX_STAR, $1);
      }
    | lunit Plus { $$ = self.heap.apply2(LEX_SEQ, $1, self.heap.apply(LEX_STAR, $1)); }
    | lunit QuestionMark { $$ = self.heap.apply(LEX_OPT, $1); }
    | lunit
    ;

lunit:
    OpenParenthesis re CloseParenthesis { $$ = $2; }
    | Constant {
        if(!isstring($1)){
          print!(
	      "{}syntax error - unexpected token \"",
	      if echoing {"\n"} else {""}
	    );
	    out(stdout,  yystack.owned_value_at(0));
	    print!("\" in regular expression\n");
	    acterror();
	  };
        $$ = if $1==NILS {
          self.heap.apply(LEX_STRING, NIL)
        } else{
          if self.heap[$1].tail==NIL {
            let hd = self.heap[$1].head;
            self.heap.apply(LEX_CHAR, hd)
          } else{
            self.heap.apply(LEX_STRING, $1)
          }
        };
      }
    | CharClass {
        if($1==NIL){
          syntax("empty character class `` cannot match\n");
        }
        $$ = if self.heap[$1].tail==NIL {
          self.heap.apply(LEX_CHAR, self.heap[$1].head)
        } else {
          self.heap.apply(LEX_CLASS, $1)
        };
      }
    | AntiCharClass {
        let cons = self.heap.cons(ANTICHARCLASS, $1);
        $$ = self.heap.apply(LEX_CLASS, cons);
      }
    | Dot { $$ = LEX_DOT; }
    | name {
        let x = lexdefs;
	loop {
	    if x == NIL { break; }
	    let hd = self.heap[x].head;
	    if self.heap[hd].head == $1 { break; }
	    x=tl[x];
	}
	if x==NIL {
	  print!(
	    "{}syntax error: undefined lexeme {} in regular expression\n",
	    if echoing {"\n"} else {""},
	    get_id( $1 )
	  );
	  acterror();
	} else {
	    let hd = self.heap[x].head;
	    $$ = self.heap[hd].tail;
	}
      }
    ;

name: Name | ConstructorName ;

qualifiers:
    exp { $$ = self.heap.cons(self.heap.cons(GUARD, $1), NIL);  }
    | generator { $$ = self.heap.cons($1, NIL);  }
    | qualifiers Semicolon generator { $$ = self.heap.cons($3, $1);   }
    | qualifiers Semicolon exp { $$ = self.heap.cons(self.heap.cons(GUARD, $3), $1);   }
    ;

generator:
    e1 Comma generator {
        /* fix syntax to disallow patlist on lhs of iterate generator */
        if (self.heap[$3].head==GENERATOR) {
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
        $$ = self.heap.cons(REPEAT, self.heap.cons(genlhs($1), $3));
        idsused = NIL;
      }
    | generator1
    ;

generator1:
    e1 LeftArrow exp {
        $$ = self.heap.cons(GENERATOR, self.heap.cons(genlhs($1), $3));
        idsused = NIL;
      }
    | e1 LeftArrow exp Comma exp DotDot {
        let p = genlhs($1);
        idsused = NIL;
        $$ = self.heap.cons(
          GENERATOR,
          self.heap.cons(
            p,
            self.heap.apply2(
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
            r = lambda(self.heap[l].tail, r);
            l = self.heap[l].head;
          }
        }
        r = label($5, r);
        /* to help locate type errors */
        declare(l, r);
        lastname = l;
      }

    | spec {
        let h = reverse(self.heap[$1].head);
        let hr = hd[tl[$1]];
        let t = tl[tl[$1]];
        while (h!=NIL && !SYNERR) {
          specify(self.heap[h].head, t, hr);
          h = self.heap[h].tail;
        }
        $$ = self.heap.cons(nill, NIL);
      }

    | AbsoluteType here typeforms indent With lspecs outdent {
        // extern word TABSTRS;
        // extern char *dicp, *dicq;
        let x=reverse($6);
        let ids = NIL;
        let tids = NIL;
        while (x!=NIL && !SYNERR) {
          specify(hd[hd[x]], self.heap.cons(tl[tl[hd[x]]], NIL), hd[tl[hd[x]]]);
          ids = self.heap.cons(hd[hd[x]], ids);
          x = self.heap[x].tail;
        }
        /* each id in specs has its id_type set to const(t, NIL) as a way
            of flagging that t is an abstract type */
        x = reverse($3);
        while (x!=NIL && !SYNERR) {
          let shfn;
          decltype(self.heap[x].head, abstract_t, undef_t, $2);
          tids = self.heap.cons(head(self.heap[x].head), tids);
          /* check for presence of showfunction */
          (void)strcpy(dicp, "show");
          (void)strcat(dicp, get_id(self.heap[tids].head));
          dicq = dicp + strlen(dicp) + 1;
          shfn = name();
          if (member(ids, shfn)) {
            t_showfn(self.heap[tids].head) = shfn;
          }
          x = self.heap[x].tail;
        }
        TABSTRS = self.heap.cons(self.heap.cons(tids, ids), TABSTRS);
        $$ = self.heap.cons(nill, NIL);
      }

    | typeform indent act1 here EqualEqual type act2 outdent {
        let x = redtvars(self.heap.apply($1, $6));
        decltype(self.heap[x].head, synonym_t, self.heap[x].tail, $4);
        $$ = self.heap.cons(nill, NIL);
      }

    | typeform indent act1 here Colon2Equal construction act2 outdent {
        let rhs = $6;
        let r_ids = $6;
        let n = 0;
        while (r_ids!=NIL) {
          r_ids = self.heap[r_ids].tail;
          n += 1;
        }
        while (rhs!=NIL && !SYNERR) {
          let h = self.heap[rhs].head;
          let t = $1;
          let stricts = NIL;
          let i = 0;
          while(tag[h]==AP) {
            if (tag[self.heap[h].tail]==AP && hd[self.heap[h].tail]==strict_t) {
              stricts = self.heap.cons(i, stricts);
              self.heap[h].tail = tl[self.heap[h].tail];
            }
            t = self.heap.apply2(arrow_t, self.heap[h].tail, t);
            h = self.heap[h].head;
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
            out_type(self.heap[rhs].head);
            printf("\" on right of ::=\n");
            acterror();
          } /* can this still happen? check later */
          if (stricts!=NIL) { /* ! operators were present */
            word k = id_val(h);
            while (stricts!=NIL) {
              k = self.heap.apply2(MKSTRICT, i - self.heap[stricts].head, k);
              stricts = self.heap[stricts].tail;
            }
            id_val(h) = k; /* overwrite id_val of original constructor */
          }
          r_ids = self.heap.cons(h, r_ids);
          rhs = self.heap[rhs].tail;
        }
        if (!SYNERR) {
          decltype($1, algebraic_t, r_ids, $4);
        }
        $$ = self.heap.cons(nill, NIL);
      }

    | indent setexp Export parts outdent {
        inexplist = 0;
        if (exports!=NIL) {
          errs = $2;
          syntax("multiple %export statements are illegal\n");
        } else {
          if ($4==NIL && exportfiles==NIL && embargoes!=NIL) {
            exportfiles = self.heap.cons(Plus, NIL);
          }
          exports = self.heap.cons($2, $4);  /* self.heap.cons(hereinfo, identifiers) */
        }
        $$ = self.heap.cons(nill, NIL);
      }

    | Free here OpenBrace specs CloseBrace {
        if (freeids!=NIL) {
          errs=$2;
          syntax("multiple %free statements are illegal\n");
        } else {
          let x = reverse($4);
          while (x!=NIL&&!SYNERR) {
            specify(hd[hd[x]], tl[tl[hd[x]]], hd[tl[hd[x]]]);
            freeids = self.heap.cons(head(hd[hd[x]]), freeids);
            if (tl[tl[hd[x]]]==type_t) {
              t_class(self.heap[freeids].head) = free_t;
            }
            else {
              id_val(self.heap[freeids].head) = FREE; /* conventional value */
            }
            x = self.heap[x].tail;
          }
          fil_share(self.heap[files].head) = 0; /* parameterised scripts unshareable */
          freeids = alfasort(freeids);
          for(x=freeids; x!=NIL; x=self.heap[x].tail){
            /* each element of freeids is of the form
              self.heap.cons(id, self.heap.cons(original_name, type)) */
            self.heap[x].head = self.heap.cons(
              self.heap[x].head,
              self.heap.cons(
                datapair(get_id(self.heap[x].head), 0),
                id_type(self.heap[x].head)
              )
            );
          }
        }
        $$ = self.heap.cons(nill, NIL);
      }

    | Include bindings modifiers outdent { /* fiddle - 'indent' done by yylex() on reading fileid */
        // extern char *dicp;
        // extern word CLASHES, BAD_DUMP;
        /* $1 contains file+hereinfo */
        includees = self.heap.cons(self.heap.cons($1, self.heap.cons($3, $2)), includees);
        $$ = self.heap.cons(nill, NIL);
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
        for(;p!=NIL;p=self.heap[p].tail){
          if (dval(self.heap[p].head)==UNDEF) {
            nonterminals = add1(dlhs(self.heap[p].head), nonterminals);
          }
          else{
            lhs = add1(dlhs(self.heap[p].head), lhs);
          }
        }
        nonterminals = setdiff(nonterminals, lhs);
        if (nonterminals!=NIL) {
          errs = $1;
          member($4, self.heap[nonterminals].head); /*||findnt(self.heap[nonterminals].head)*/
          printf("%sfatal error in grammar, ", echoing?"\n":"");
          printf(
            "undefined nonterminal%s: ",
            if self.heap[nonterminals].tail==NIL {""} else {"s"}
          );
          printlist("", nonterminals);
          acterror();
        } else { /* compute list of nonterminals admitting empty prodn */
          eprodnts = NIL;
          L:
          for(p = $6; p!=NIL; p = self.heap[p].tail) {
            if(!member(eprodnts, dlhs(self.heap[p].head)) && eprod(dval(self.heap[p].head))) {
              eprodnts = self.heap.cons(dlhs(self.heap[p].head), eprodnts);
              goto L;
            }
          }
          /*
            now compute startswith reln between nonterminals
            (performing binomial transformation en route)
            and use to detect unremoved left recursion
          */
          for(p=$6;p!=NIL;p=self.heap[p].tail) {
            lhs = starts(dval(self.heap[p].head));
            if (member(lhs, dlhs(self.heap[p].head))) {
              binom(dval(self.heap[p].head), dlhs(self.heap[p].head));
              startswith = self.heap.cons(
                self.heap.cons(
                  dlhs(self.heap[p].head),
                  starts(dval(self.heap[p].head))
                ),
                startswith
              );
            } else {
              startswith = self.heap.cons(self.heap.cons(dlhs(self.heap[p].head), lhs), startswith);
            }
          }
          startswith = tclos(sortrel(startswith));
          for(; startswith!=NIL; startswith = self.heap[startswith].tail) {
            if (member(tl[hd[startswith]], hd[hd[startswith]])) {
              leftrecs = add1(hd[hd[startswith]], leftrecs);
            }
          }
          if(leftrecs!=NIL){
            errs = getloc(self.heap[leftrecs].head, $6);
            printf(
              "%sfatal error in grammar, ",
              if echoing {"\n"} else {""}
            );
            printlist("irremovable left recursion: ", leftrecs);
            acterror();
          }
          if($4==NIL) { /* implied start symbol */
            $4 = self.heap.cons(dlhs(self.heap[lastlink($6)].head), NIL);
          }
          fnts = 1; /* fnts is flag indicating %bnf in use */
          if (self.heap[$4].tail==NIL) { /* only one start symbol */
            subjects = getfname(self.heap[$4].head);
            body = self.heap.apply2(G_CLOSE, str_conv(get_id(self.heap[$4].head)), self.heap[$4].head);
          } else {
            body = Void;
            subjects = Void;
            while ($4!=NIL) {
              subjects = pair(getfname(self.heap[$4].head), subjects);
              body = pair(
                self.heap.apply2(G_CLOSE, str_conv(get_id(self.heap[$4].head)), self.heap[$4].head),
                body
              );
              $4 = self.heap[$4].tail;
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
    bindingseq binding { $$ = self.heap.cons($2, $1); }
    | binding { $$ = self.heap.cons($1, NIL); }
    ;

binding:
    Name indent Equal exp outdent { $$ = self.heap.cons($1, $4); }
    | typeform indent act1 EqualEqual type act2 outdent {
        let x = redtvars(self.heap.apply($1, $5));
        let arity = 0;
        let h = self.heap[x].head;
        while (tag[h]==AP) {
          arity += 1;
          h = self.heap[h].head;
        }
        $$ = self.heap.apply(h, make_typ(arity, 0, synonym_t, self.heap[x].tail));
      }
    ;

modifiers:
    /* empty */ { $$ = NIL; }
    | negmods {
        let a;
        let b;
        let c = 0;
        for(a = $1; a!=NIL; a = self.heap[a].tail) {
          for(b = self.heap[a].tail; b!=NIL; b = self.heap[b].tail) {
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
    negmods negmod { $$ = self.heap.cons($2, $1); }
    | negmod { $$ = self.heap.cons($1, NIL); }
    ;

negmod:
    Name Divide Name { $$ = self.heap.cons($1, $3); }
    | ConstructorName Divide ConstructorName { $$ = self.heap.cons($1, $3); }
    | Minus Name { $$ = self.heap.cons(make_pn(UNDEF), $2); }
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
        $$ = self.heap.cons($1, NIL);
        dval($1) = tries(dlhs($1), self.heap.cons(dval($1), NIL));
        if (!SYNERR && get_ids(dlhs($1))==NIL) {
          errs = hd[hd[tl[dval($1)]]];
          syntax("illegal lhs for local definition\n");
        }
      }
    | ldefs ldef {
        if(dlhs($2)==dlhs(self.heap[$1].head) /*&&dval(self.heap[$1].head)!=UNDEF*/) {
          $$ = $1;
          if (!fallible(self.heap[self.heap[dval(hd[$1].head.tail)]])) {
            errs = self.heap[dval($2)].head;
            printf(
              "%ssyntax error: unreachable case in defn of \"%s\"\n",
              if echoing {"\n"} else {""},
              get_id(dlhs($2))
            );
            acterror();
          }
          self.heap[dval(self.heap[$1].head.tail)] = self.heap.cons(dval($2), self.heap[dval(self.heap[$1].head.tail)]);
        } else if (!SYNERR) {
          let ns = get_ids(dlhs($2));
          let hr = self.heap[dval($2)].head;
          if(ns==NIL){
            errs = hr;
            syntax("illegal lhs for local definition\n");
          }
          $$ = self.heap.cons($2, $1);
          dval($2) = tries(dlhs($2), self.heap.cons(dval($2), NIL));
          while(ns!=NIL&&!SYNERR) { /* local nameclash check */
            nclashcheck(self.heap[ns].head, $1, hr);
            ns = self.heap[ns].tail;
          } /* potentially quadratic - fix later */
        }
      }
   ;

ldef:
    spec {
        errs = hd[tl[$1]];
        syntax("`::' encountered in local defs\n");
        $$ = self.heap.cons(nill, NIL);
      }
    | typeform here EqualEqual {
        errs=$2;
        syntax("`==' encountered in local defs\n");
        $$ = self.heap.cons(nill, NIL);
      }
    | typeform here Colon2Equal {
        errs=$2;
        syntax("`::=' encountered in local defs\n");
        $$ = self.heap.cons(nill, NIL);
      }
    | v act2 indent Equal here rhs outdent {
        let l = $1;
        let r = $6;
        word f = head(l);
        if(tag[f]==ID&&!isconstructor(f)) { /* fnform defn */
          while(tag[l]==AP){
            r = lambda(self.heap[l].tail, r);
            l = self.heap[l].head;
          }
        }
        r = label($5, r); /* to help locate type errors */
        $$ = defn(l, undef_t, r);
      }
    ;

vlist:
    v { $$ = self.heap.cons($1, NIL); }
    | vlist Comma v /* left recursive so as not to eat YACC stack */ {
        /* reverse order, NB */
        $$ = self.heap.cons($3, $1);
      }
    ;

v:
    v1
    | v1 Colon v { $$ = self.heap.cons($1, $3); }
    ;

v1:
    v1 Plus Constant  /* n+k pattern */ {
        if (!isnat($3)){
          syntax("inappropriate use of \"+\" in pattern\n");
        }
        $$ = self.heap.apply2(Plus, $3, $1);
      }
    | Minus Constant {
        /* if(tag[$2]==DOUBLE)
              $$ = self.heap.cons(Constant, sto_dbl(-get_dbl($2))); else */
        if (tag[$2]==INT) {
          $$ = self.heap.cons(Constant, bignegate($2));
        } else {
          syntax("inappropriate use of \"-\" in pattern\n");
        }
      }
    | v2 InfixName v1 { $$ = self.heap.apply2($2, $1, $3); }
    | v2 InfixCName v1 { $$ = self.heap.apply2($2, $1, $3); }
    | v2;

v2:
    v3
    | v2 v3 {
        $$ = self.heap.apply(
          if self.heap[$1].head==Constant && tag[tl[$1]]==ID {
            self.heap[$1].tail
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
          $$ = self.heap.cons(Constant, $1);
        } /* picks up repeated names in a template */
        else {
          idsused = self.heap.cons($1, idsused);
        }
      }
    | ConstructorName
    | Constant {
        if (tag[$1]==DOUBLE) {
          syntax("use of floating point literal in pattern\n");
        }
        $$ = self.heap.cons(Constant, $1);
      }
    | OpenBracket CloseBracket { $$ = nill; }
    | OpenBracket vlist CloseBracket {
        let x = $2;
        let y = nill;
        while(x!=NIL) {
          y = self.heap.cons(self.heap[x].head, y);
          x = self.heap[x].tail;
        }
        $$ = y;
      }
    | OpenParenthesis CloseParenthesis { $$ = Void; }
    | OpenParenthesis v CloseParenthesis { $$ = $2; }
    | OpenParenthesis v Comma vlist CloseParenthesis {
        /* representation of the tuple (a1, ..., an) is
             tself.heap.cons(a1, tself.heap.cons(a2, ...pair(a(n-1), an))) */
        if(self.heap[$4].tail==NIL) {
          $$=pair($2, self.heap[$4].head);
        } else {
          $$ = pair(hd[tl[$4]], self.heap[$4].head);
          $4 = tl[tl[$4]];
          while ($4!=NIL) {
            $$=tself.heap.cons(self.heap[$4].head, $$);
            $4=self.heap[$4].tail;
          }
          $$ = tself.heap.cons($2, $$);
        }

      }
    ;

type:
    type1
    | type Arrow type { $$ = self.heap.apply2(arrow_t, $1, $3); }
    ;

type1:
    type2 InfixName type1 { $$ = self.heap.apply2($2, $1, $3); }
    | type2
    ;

type2:
    /* type2 argtype  /* too permissive - fix later */
        /* = { $$ = self.heap.apply($1, $2); }| */
    tap
    | argtype
    ;

tap:
    Name argtype { $$ = self.heap.apply($1, $2); }
    | tap argtype { $$ = self.heap.apply($1, $2); }
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
    | OpenBracket type CloseBracket  /* at release one was `typelist' */ { $$ = self.heap.apply(list_t, $2); }
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
          y = self.heap.apply2(comma_t, self.heap[x].head, y);
          x = self.heap[x].tail;
        }
        $$ = self.heap.apply2(comma_t, $1, y);
      }
    ;

typel:
    type { $$ = self.heap.cons($1, NIL); }
    | typel Comma type /* left recursive so as not to eat YACC stack */ { $$ = self.heap.cons($3, $1); }
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
        exportfiles = self.heap.cons(Plus, exportfiles);
      }
    | Name { $$ = add1($1, NIL); }
    | Minus Name {
        $$ = NIL;
        embargoes = add1($2, embargoes);
      }
    | PathName { $$ = NIL; }
    | Plus {
        $$ = NIL;
        exportfiles=self.heap.cons(Plus, exportfiles);
      }
    ;

specs:  /* returns a list of self.heap.cons(id, self.heap.cons(here, type))
           in reverse order of appearance */
    specs spec {
        let x = $1;
        let h = self.heap[$2].head;
        let t = self.heap[$2].tail;
        while (h!=NIL) {
          x = self.heap.cons(self.heap.cons(self.heap[h].head, t), x);
          h = self.heap[h].tail;
        }
        $$ = x;
      }
    | spec {
        let x = NIL;
        let h = self.heap[$1].head;
        let t = self.heap[$1].tail;
        while (h!=NIL) {
          x = self.heap.cons(self.heap.cons(self.heap[h].head, t), x);
          h = self.heap[h].tail;
        }
        $$ = x;
      }
    ;

spec:
    typeforms indent here ColonColon ttype outdent {
        $$ = self.heap.cons($1, self.heap.cons($3, $5));
      } /* hack: `typeforms' includes `namelist' */
    ;

lspecs:  /* returns a list of self.heap.cons(id, self.heap.cons(here, type))
           in reverse order of appearance */
    lspecs lspec {
        let x = $1;
        let h = self.heap[$2].head;
        let t = self.heap[$2].tail;
        while(h!=NIL) {
          x = self.heap.cons(self.heap.cons(self.heap[h].head, t), x);
          h = self.heap[h].tail;
        }
        $$ = x;
      }
    | lspec {
        let x = NIL;
        let h = self.heap[$1].head;
        let t = self.heap[$1].tail;
        while(h!=NIL) {
          x = self.heap.cons(self.heap.cons(self.heap[h].head, t), x);
          h = self.heap[h].tail;
        }
        $$ = x;
      }
    ;

lspec:
    namelist indent here {inbnf=0;} ColonColon type outdent { $$ = self.heap.cons($1, self.heap.cons($3, $6)); };

namelist:
    Name Comma namelist { $$ = self.heap.cons($1, $3); }
    | Name { $$ = self.heap.cons($1, NIL); }
    ;

typeforms:
    typeforms Comma typeform act2 { $$ = self.heap.cons($3, $1); }
    | typeform act2 { $$ = self.heap.cons($1, NIL); }
    ;

typeform:
    ConstructorName typevars { syntax("upper case identifier out of context\n"); }
    | Name typevars   /* warning if typevar is repeated */ {
        $$ = $1;
        idsused = $2;
        while($2!=NIL) {
          $$ = self.heap.apply($$, self.heap[$2].head);
          $2 = self.heap[$2].tail;
        }
      }
    | typevar InfixName typevar {
        if(eqtvar($1, $3)){
          syntax("repeated type variable in typeform\n");
        }
        idsused = self.heap.cons($1, self.heap.cons($3, NIL));
        $$ = self.heap.apply2($2, $1, $3);
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
        $$ = self.heap.cons($1, $2);
      }
    ;

construction:
    constructs { /* keeps track of sui-generis constructors */
        // extern word SGC;
        if ( self.heap[$1].tail==NIL && tag[hd[$1]]!=ID ) {
                        /* 2nd conjunct excludes singularity types */
          SGC = self.heap.cons(head(self.heap[$1].head), SGC);
        }
      }
    ;

constructs:
    construct { $$ = self.heap.cons($1, NIL); }
    | constructs Pipe construct { $$ = self.heap.cons($3, $1); }
    ;

construct:
    field here InfixCName field {
        $$ = self.heap.apply2($3, $1, $4);
        id_who($3) = $2;
      }
    | construct1
    ;

construct1:
    OpenParenthesis construct CloseParenthesis { $$ = $2; }
    | construct1 field1 { $$ = self.heap.apply($1, $2); }
    | here ConstructorName {
        $$ = $2;
        id_who($2) = $1;
      }
    ;

field:
    type
    | argtype Bang { $$ = self.heap.apply(strict_t, $1); }
    ;

field1:
    argtype Bang { $$ = self.heap.apply(strict_t, $1); }
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
          self.heap.cons($2, $1)
        };
      }
    ;

productions:
    lspec {
        let h = reverse(self.heap[$1].head);
        let hr = hd[tl[$1]];
        let t = tl[tl[$1]];
        inbnf = 1;
        $$ = NIL;
        while (h!=NIL && !SYNERR) {
          ntspecmap = self.heap.cons(self.heap.cons(self.heap[h].head, hr), ntspecmap);
          $$ = add_prod(defn(self.heap[h].head, t, UNDEF), $$, hr);
          h = self.heap[h].tail;
        }
      }
    | production { $$ = self.heap.cons($1, NIL); }
    | productions lspec {
        let h = reverse(self.heap[$2].head);
        let hr = hd[tl[$2]];
        let t = tl[tl[$2]];
        inbnf = 1;
        $$=$1;
        while(h!=NIL&&!SYNERR){
          ntspecmap = self.heap.cons(self.heap.cons(self.heap[h].head, hr), ntspecmap);
          $$ = add_prod(defn(self.heap[h].head, t, UNDEF), $$, hr);
          h = self.heap[h].tail;
        }
      }
    | productions production { $$ = add_prod($2, $1, self.heap[dval($2)].head); }
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
    error_term { $$ = self.heap.apply2(G_ERROR, G_ZERO, $1); }
    | phrase1 {
        $$ = self.heap[$1].head;
        $1 = self.heap[$1].tail;
        while ($1!=NIL) {
          $$ = label(self.heap[$1].head, $$);
          $1 = self.heap[$1].tail;
          $$ = self.heap.apply2(G_ALT, self.heap[$1].head, $$);
          $1 = self.heap[$1].tail;
        }
      }
    | phrase1 Pipe error_term {
        $$=self.heap[$1].head;
        $1=self.heap[$1].tail;
        while($1!=NIL){
          $$ = label(self.heap[$1].head, $$);
          $1 = self.heap[$1].tail;
          $$ = self.heap.apply2(G_ALT, self.heap[$1].head, $$);
          $1 = self.heap[$1].tail;
        }
        $$ = self.heap.apply2(G_ERROR, $$, $3);
      }
      /* we right rotate G_ALT's to facilitate left factoring (see trans) */
    ;

phrase1:
    term { $$=self.heap.cons($1, NIL); }
    | phrase1 Pipe here term { $$ = self.heap.cons($4, self.heap.cons($3, $1)); }
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
    factor { $$ = self.heap.cons($1, NIL); }
    | factors factor {
        if (self.heap[$1].head==G_END) {
          syntax("unexpected token after end\n");
        }
        $$ = self.heap.cons($2, $1);
      }
    ;

factor:
    unit
    | OpenBrace unit CloseBrace {
        $$ = self.heap.apply(outdent_fn, self.heap.apply2(indent_fn, getcol_fn(), $2));
      }
    | OpenBrace unit {
        obrct += 1;
        $$ = self.heap.apply2(indent_fn, getcol_fn(), $2);
      }
    | unit CloseBrace {
        obrct -= 1;
	      if obrct < 0 {
	        syntax("unmatched `}' in grammar rule\n");
	      }
        $$ = self.heap.apply(outdent_fn, $1);
      }
    ;

unit:
    symbol
    | symbol Times { $$ = self.heap.apply(G_STAR, $1); }
    | symbol Plus {
        $$ = self.heap.apply2(G_SEQ, $1, self.heap.apply2(G_SEQ, self.heap.apply(G_STAR, $1), self.heap.apply(G_RULE, self.heap.apply(C, P))));
      }
    | symbol QuestionMark { $$ = self.heap.apply(G_OPT, $1); }
    ;

symbol:
    Name {
        //extern word NEW;
        nonterminals = newadd1($1, nonterminals);
        if(NEW){
          ntmap = self.heap.cons(self.heap.cons($1, lasth), ntmap);
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
        $$ = self.heap.apply(G_SYMB, $1);
      }
    | Caret { $$=G_STATE; }
    | {inbnf=0;} OpenBracket exp {inbnf=1;} CloseBracket { $$ = self.heap.apply(G_SUCHTHAT, $3); }
    | Minus { $$ = G_ANY; }
    ;

%%
/*  end of Miranda rules  */


impl Parser {

	// Todo: Do we need mut versions? We take `&mut self` but only return `&HeapCell.`
	fn hd_hd(&mut self, idx: Into<usize>) -> &mut HeapCell {
	  let hd = self.heap[x.into()].head;
	  self.heap[hd.into()].head
	}

	fn hd_tl(&mut self, idx: Into<usize>) -> &mut HeapCell {
	  let hd = self.heap[x.into()].head;
	  self.heap[hd.into()].tail
	}

	fn tl_tl(&mut self, idx: Into<usize>) -> &mut HeapCell {
	  let hd = self.heap[x.into()].tail;
	  self.heap[hd.into()].tail
	}

	fn tl_hd(&mut self, idx: Into<usize>) -> &mut HeapCell {
	  let hd = self.heap[x.into()].tail;
	  self.heap[hd.into()].head
	}



}
