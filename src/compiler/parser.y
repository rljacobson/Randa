/*
    Bison specification file for a parser for Miranda written in Rust.

*/

%define api.parser.struct {Parser}
%define api.parser.generic {<'ctx /* 'fix quotes */>}
%define api.value.type {Value}
%define api.parser.check_debug { self.debug }
%start parse_entry

%define parse.error custom
%define parse.trace

%code use {
// all use goes here
use super::{
  token::{ParserLookahead},
  lexer::Lexer,
  HereInfo,
  Loc,
  ParserActivation,
  ParserDiagnostic,
  ParserConstructorPayload,
  ParserDefinitionPayload,
  ParserExportDirectivePayload,
  ParserFreeBindingPayload,
  ParserIncludeDirectivePayload,
  ParserRunDiagnostics,
  ParserRunResult,
  ParserSpecificationPayload,
  ParserTopLevelDirectivePayload,
  ParserTopLevelScriptPayload,
  ParserTypeDeclarationPayload,
  Token,
  ParserVmContext,
};

use crate::{
    big_num::IntegerRef,
    data::{
        api::{ConsList, FileInfoRef, HeapObjectProxy, IdentifierValueTypeKind},
        tag::Tag,
        values::{
            Value,
            RawValue
        },
        combinator::Combinator,
        Heap,
        Type,
    }
};

}

%code parser_fields {
  /// Gives access to the end result of parsing.
  result: Option<Value>,
  /// Enables debug printing
  pub debug: bool,
  /// The heap on which everything is constructed.
  heap: &'ctx /* 'fix quotes */ mut Heap,
  vm: ParserVmContext,
  syntax_error_found: bool,
  diagnostics: ParserRunDiagnostics,
  top_level_script_parsed: bool,
  directive_include_requests: Vec<ParserIncludeDirectivePayload>,
  definition_payloads: Vec<ParserDefinitionPayload>,
  specification_payloads: Vec<ParserSpecificationPayload>,
  type_declaration_payloads: Vec<ParserTypeDeclarationPayload>,
  constructor_payloads: Vec<ParserConstructorPayload>,
  free_binding_payloads: Vec<ParserFreeBindingPayload>,

  lex_states: RawValue,
  used_identifiers: RawValue,
  lex_rule_definitions: RawValue,
  last_identifier: RawValue,
  inherited_attributes: RawValue,
  nonterminals: RawValue,
  empty_production_nonterminals: RawValue,
  nonterminal_specification_map: RawValue,
  nonterminal_map: RawValue,
  last_diagnostic_location: RawValue,

  bnf_mode: u8,   // 0=false, 1=true, 2=something about offside rule
  export_list_mode: bool,
  lex_mode: u8,   // 0=false, 1=true, 2=??
  type_variable_scope: bool,

  semantic_reduction_count: i16,
  open_bracket_count: i16,

  exported_identifiers: RawValue,
  export_path_requests: RawValue,
  export_embargoes: RawValue,
  include_requests: RawValue,
  free_identifiers: RawValue,

  bnf_enabled: RawValue,
  identifier_dictionary: String,
  constructor_dictionary: String,
}

%code {
  // code in a generic %code block.
}

%token Value Eval Where If To LeftArrow ColonColon Colon2Equal Identifier
    TypeVar Name Constant ConstructorName DollarDollar Offside ElseEqual
    AbsoluteType With Diagonal EqualEqual Free Include Export Type Otherwise
    Show PathName BNF Lex EndIR ErrorSymbol EndSymbol EmptySymbol ReadVals LexDef
    CharClass AntiCharClass Begin RightArrow PlusPlus MinusMinus DotDot Vel
    GreaterEqual NotEqual LessEqual Remainder DivideInteger InfixName
    InfixCName Colon Ampersand Greater Equal Less Plus Minus Times DivideFloat
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

// Used so often.
static NIL: Value = Value::Combinator(Combinator::Nil);
const NIL_RAW: RawValue = Combinator::Nil as RawValue;
const GUARD: Value = Value::Data(1);

/*
Miranda has text representation of tokens here, but we don't need that,
because we defined them in `Token::name()`.
    char *yysterm[]= {…}


Miranda has all of this global state:

  extern word listdiff_fn, indent_fn, outdent_fn;
  word lastname=0;
  word idsused=NIL;
  word tvarscope=0;
  word includees=NIL, embargoes=NIL, exportfiles=NIL, freeids=NIL, exports=NIL;
  word lexdefs=NIL, lexstates=NIL, inlex=0, inexplist=0;
  word inbnf=0, col_fn=0, fnts=NIL, eprodnts=NIL, nonterminals=NIL, sreds=0;
  word ihlist=0, ntspecmap=NIL, ntmap=NIL, lasth=0;
  word open_bracket_count=0;

  // Global state in Miranda that is apparently not used.
  word suppressids=NIL;



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

parse_entry:
    exp {
        self.result = Some($1);
        $$ = $1;
      }
    | top_level_script {
        self.top_level_script_parsed = true;
        $$ = NIL;
      }
    ;

entity:  /* the entity to be parsed is either a definition script or an
            expression (the latter appearing as a command line) */

    error

    | script {
        /* outstats(); */  // statistics not usually wanted after compilation
      }

          /*
          The following relate to interactive sessions. See
              https://www.cs.kent.ac.uk/people/staff/dat/miranda/manual/4.html
          for the different commands for interactive sessions.
          */
    | Value exp {
        // Interactive `$+` / `lastexp` update is handled outside the current
        // expression-only parser path.
      }

    | Eval exp {
        // Interactive evaluation is handled outside the parser.
      }

    | Eval exp ColonColon {
        // Interactive type reporting is handled outside the parser.
      }

    | Eval exp To {
          // Interactive output redirection is handled outside the parser.
      }
    ;

script:
    /* script can be empty */
    | defs { $$ = $1; }
    ;

exp:
    op /* will later suppress in favour of (op) in arg */ { $$ = $1; }
    | e1 { $$ = $1; }
    ;

/* The active top-level script slice currently accepts `%include`, `%export`,
   and top-level value/function definitions. Other top-level forms are still
   classified before parser entry and follow the deferred integration path. */
top_level_script:
    top_level_item { $$ = NIL; }
    | top_level_script top_level_item { $$ = NIL; }
    ;

top_level_item:
    include_export_item { $$ = NIL; }
    | top_level_definition_item { $$ = NIL; }
    | top_level_specification_item { $$ = NIL; }
    | top_level_synonym_type_item { $$ = NIL; }
    | top_level_algebraic_type_item { $$ = NIL; }
    | top_level_free_item { $$ = NIL; }
    ;

top_level_definition_anchor:
    /* empty */ {
        let here_info = HereInfo::from_source_location(
          self.yylexer.source_name(),
          self.yylexer.source_text(),
          Some(self.yylexer.current_loc())
        );
        let anchor = FileInfoRef::from_script_file(
          self.heap,
          here_info.script_file,
          here_info.line_number,
        );
        $$ = Value::Reference(anchor.get_ref());
      }
    ;

top_level_type_expr:
    Name { $$ = $1; }
    | Type { $$ = Type::Type.into(); }
    ;

top_level_free_type_expr:
    Name { $$ = $1; }
    ;

top_level_constructor_list:
    ConstructorName { $$ = self.heap.cons_ref($1, NIL); }
    | top_level_constructor_list Pipe ConstructorName { $$ = self.heap.cons_ref($3, $1); }
    ;

top_level_free_item:
    Free OpenBrace Name top_level_definition_anchor ColonColon top_level_free_type_expr CloseBrace directive_terminators {
        let identifier = $3;
        let anchor = $4;
        let type_expr = $6;
        if !self.free_binding_payloads.is_empty() {
          self.syntax("multiple %free statements are illegal\n");
        } else {
          self.free_binding_payloads.push(ParserFreeBindingPayload {
            identifier: identifier.into(),
            type_expr,
            anchor: anchor.into(),
          });
        }
        $$ = NIL;
      }
    ;

include_export_item:
    export_directive directive_terminators { $$ = NIL; }
    | include_directive directive_terminators { $$ = NIL; }
    ;

top_level_definition_item:
    Name top_level_definition_anchor Equal exp directive_terminators {
        let identifier = $1;
        self.definition_payloads.push(ParserDefinitionPayload {
          identifier: identifier.into(),
          body: $4.into(),
          anchor: $2.into(),
        });
        self.last_identifier = identifier.into();
        $$ = NIL;
      }
    ;

top_level_specification_item:
    Name top_level_definition_anchor ColonColon top_level_type_expr directive_terminators {
        let identifier = $1;
        let anchor = $2;
        let type_expr = $4;
        self.specification_payloads.push(ParserSpecificationPayload {
          identifier: identifier.into(),
          type_expr,
          anchor: anchor.into(),
        });
        $$ = NIL;
      }
    ;

top_level_synonym_type_item:
    Name top_level_definition_anchor EqualEqual top_level_type_expr directive_terminators {
        let type_identifier = $1;
        let anchor = $2;
        let info = $4;
        self.type_declaration_payloads.push(ParserTypeDeclarationPayload {
          type_identifier: type_identifier.into(),
          kind: IdentifierValueTypeKind::Synonym,
          info,
          anchor: anchor.into(),
        });
        $$ = NIL;
      }
    ;

top_level_algebraic_type_item:
    Name top_level_definition_anchor Colon2Equal top_level_constructor_list directive_terminators {
        let type_identifier = $1;
        let anchor = $2;
        let constructor_list = $4;
        let mut constructors: RawValue = constructor_list.into();
        while constructors!=NIL_RAW {
          self.constructor_payloads.push(ParserConstructorPayload {
            constructor: self.heap[constructors].head,
            parent_type: type_identifier.into(),
            anchor: anchor.into(),
          });
          constructors = self.heap[constructors].tail;
        }
        self.type_declaration_payloads.push(ParserTypeDeclarationPayload {
          type_identifier: type_identifier.into(),
          kind: IdentifierValueTypeKind::Algebraic,
          info: constructor_list,
          anchor: anchor.into(),
        });
        $$ = NIL;
      }
    ;

export_anchor:
    Export {
        self.export_list_mode = true;
        let here_info = HereInfo::from_source_location(
          self.yylexer.source_name(),
          self.yylexer.source_text(),
          Some(self.yylexer.current_loc())
        );
        let anchor = FileInfoRef::from_script_file(
          self.heap,
          here_info.script_file,
          here_info.line_number,
        );
        $$ = Value::Reference(anchor.get_ref());
      }
    ;

export_directive:
    export_anchor export_items {
        self.export_list_mode = false;
        let export_parts = $2;
        if self.exported_identifiers != NIL_RAW {
          self.syntax("multiple %export statements are illegal\n");
        } else {
          if export_parts == NIL && self.export_path_requests == NIL_RAW && self.export_embargoes != NIL_RAW {
            self.export_path_requests = self.heap.cons_ref(Combinator::Plus.into(), NIL).into();
          }
          self.exported_identifiers = self.heap.cons_ref($1, export_parts).into();
        }
        $$ = NIL;
      }
    ;

export_items:
    export_items Name { $$ = self.heap.cons_ref($2, $1); }
    | export_items Minus Name {
        $$ = $1;
        self.export_embargoes = self.heap.cons_ref($3, self.export_embargoes.into()).into();
      }
    | export_items StringLiteral {
        $$ = $1;
        self.export_path_requests = self.heap.cons_ref($2, self.export_path_requests.into()).into();
      }
    | export_items Plus {
        $$ = $1;
        self.export_path_requests = self.heap.cons_ref(Combinator::Plus.into(), self.export_path_requests.into()).into();
      }
    | Name { $$ = self.heap.cons_ref($1, NIL); }
    | Minus Name {
        $$ = NIL;
        self.export_embargoes = self.heap.cons_ref($2, self.export_embargoes.into()).into();
      }
    | StringLiteral {
        $$ = NIL;
        self.export_path_requests = self.heap.cons_ref($1, self.export_path_requests.into()).into();
      }
    | Plus {
        $$ = NIL;
        self.export_path_requests = self.heap.cons_ref(Combinator::Plus.into(), self.export_path_requests.into()).into();
      }
    ;

include_directive:
    Include include_target include_bindings include_modifiers {
        let target_ref: RawValue = $2.into();
        let target_path = self.heap[target_ref].head;
        let anchor = self.heap[target_ref].tail;
        self.directive_include_requests.push(ParserIncludeDirectivePayload {
          anchor,
          target_path,
          modifiers: $4.into(),
          bindings: $3.into(),
        });
        $$ = NIL;
      }
    ;

include_target:
    StringLiteral {
        let here_info = HereInfo::from_source_location(
          self.yylexer.source_name(),
          self.yylexer.source_text(),
          Some(self.yylexer.current_loc())
        );
        let anchor = FileInfoRef::from_script_file(
          self.heap,
          here_info.script_file,
          here_info.line_number,
        );
        $$ = self.heap.cons_ref($1, Value::Reference(anchor.get_ref()));
      }
    ;

include_bindings:
    /* empty */ { $$ = NIL; }
    | OpenBrace include_binding_sequence CloseBrace { $$ = $2; }
    ;

include_binding_sequence:
    include_binding_sequence include_binding { $$ = self.heap.cons_ref($2, $1); }
    | include_binding { $$ = self.heap.cons_ref($1, NIL); }
    ;

include_binding:
    Name Equal exp { $$ = self.heap.cons_ref($1, $3); }
    ;

include_modifiers:
    /* empty */ { $$ = NIL; }
    | include_modifier_list { $$ = $1; }
    ;

include_modifier_list:
    include_modifier_list include_modifier { $$ = self.heap.cons_ref($2, $1); }
    | include_modifier { $$ = self.heap.cons_ref($1, NIL); }
    ;

include_modifier:
    Name Divide Name { $$ = self.heap.cons_ref($1, $3); }
    | ConstructorName Divide ConstructorName { $$ = self.heap.cons_ref($1, $3); }
    | Minus Name {
        let suppression_marker = self.heap.make_private_symbol_ref(Combinator::Undef.into());
        $$ = self.heap.cons_ref(suppression_marker, $2);
      }
    ;

directive_terminators:
    /* empty */ { $$ = NIL; }
    | directive_terminators Newline { $$ = NIL; }
    | directive_terminators separator { $$ = NIL; }
    ;

op:
    Tilde { $$ = Combinator::Not.into(); }
    | Hash { $$ = Combinator::Length.into(); }
    | diop { $$ = $1; }
    ;

diop:
    Minus { $$ = Combinator::Minus.into(); }
    | diop1 { $$ = $1; }
    ;

diop1:
    Plus { $$ = Combinator::Plus.into(); }
    | PlusPlus { $$ = Combinator::Append.into(); }
    | Colon { $$ = Combinator::P.into(); }
    | MinusMinus { $$ = self.vm.listdiff_function(); }
    | Vel { $$ = Combinator::Or.into(); }
    | Ampersand { $$ = Combinator::And.into(); }
    | relop { $$ = $1; }
    | Times { $$ = Combinator::Times.into(); }
    | DivideFloat { $$ = Combinator::DivideFloat.into(); }
    | DivideInteger { $$ = Combinator::DivideInteger.into(); }
    | Remainder { $$ = Combinator::Remainder.into(); }
    | Caret { $$ = Combinator::Power.into(); }
    | Dot { $$ = Combinator::B.into(); }
    | Bang { $$ = self.heap.apply_ref(Combinator::C.into(), Combinator::Subscript.into()); }
    | InfixName { $$ = $1; }
    | InfixCName { $$ = $1; }
    ;

relop:
    Greater { $$ = Combinator::Gr.into(); }
    | GreaterEqual { $$ = Combinator::Gre.into(); }
    | eqop { $$ = Combinator::Eq.into(); }
    | NotEqual { $$ = Combinator::NEq.into(); }
    | LessEqual { $$ = self.heap.apply_ref(Combinator::C.into(), Combinator::Gre.into()); }
    | Less { $$ = self.heap.apply_ref(Combinator::C.into(), Combinator::Gr.into()); }
    ;

eqop:
    EqualEqual { $$ = NIL; } /* silently accept for benefit of Haskell users */
    | Equal { $$ = NIL; }
    ;

rhs:
    cases Where ldefs { $$ = block($3, compose($1), 0); }
    | exp Where ldefs { $$ = block($3, $1, 0); }
    | exp { $$ = $1; }
    | cases { $$ = compose($1); }
    ;

cases:
    exp Comma if exp { $$ = self.heap.cons_ref(self.heap.apply2(Combinator::Cond.into(), $4, $1).into(), NIL); }
    | exp Comma Otherwise {
        let ow = self.heap.apply_ref(Token::Otherwise.into(), $1);
        $$ = self.heap.cons_ref(ow, NIL);
    }
    | cases reindent ElseEqual alt {
        $$ = self.heap.cons_ref($4, $1);
        let case_head = self.heap[$1].head;
        if self.heap[case_head].head == Token::Otherwise.into() {
          syntax("\"otherwise\" must be last case\n");
        }
      }
    ;

alt:
    here exp {
        errs=$1;
        syntax("obsolete syntax, \", otherwise\" missing\n");
        $$ = self.heap.apply_ref(Token::Otherwise.into(), self.heap.label_ref($1, $2));
      }
    | here exp Comma if exp { $$ = self.heap.label_ref($1, self.heap.apply2(Combinator::Cond.into(), $5, $2)); }
    | here exp Comma Otherwise { $$ = self.heap.apply_ref(Token::Otherwise.into(), self.heap.label_ref($1, $2)); }
    ;

if:
    /* empty */ {
        // The legacy `strictif` policy hook is not wired in here.
        $$ = NIL;
      }
    | If { $$ = $1; }
    ;

indent:
    /* empty */ {
        if !self.syntax_error_found {
          self.vm.layout_partial();
          self.vm.set_left_margin_partial();
        }
        $$ = NIL;
      }
    ;
/* note that because of yacc's one symbol look ahead, indent must usually be
   invoked one symbol earlier than the non-terminal to which it applies
   - see `production:' for an exception */

outdent:
    separator {
        self.vm.unset_left_margin_partial();
        $$ = NIL;
      }
    ;

separator:
    Offside { $$ = $1; }
    | Semicolon { $$ = $1; }
    ;

reindent:
    /* empty */ {
        if(!SYNERR) {
          unsetlmargin();
          layout();
          setlmargin();
        }
        $$ = NIL;
      }
    ;

liste:  /* NB - returns list in reverse order */
    exp { $$ = self.heap.cons_ref($1, NIL); }
    | liste Comma exp  /* left recursive so as not to eat YACC stack */
        { $$ = self.heap.cons_ref($3, $1); }
    ;

e1:
    Tilde e1 %prec Equal { $$ = self.heap.apply_ref(Combinator::Not.into(), $2); }
    | e1 PlusPlus e1 { $$ = self.heap.apply2(Combinator::Append.into(), $1, $3); }
    | e1 Colon e1 { $$ = self.heap.cons_ref($1, $3); }
    | e1 MinusMinus e1 { $$ = self.heap.apply2(self.vm.listdiff_function(), $1, $3);  }
    | e1 Vel e1 { $$ = self.heap.apply2(Combinator::Or.into(), $1, $3); }
    | e1 Ampersand e1 { $$ = self.heap.apply2(Combinator::And.into(), $1, $3); }
    | reln { $$ = $1; }
    | e2 { $$ = $1; }
    ;

es1:                     /* e1 or presection */
    Tilde e1 %prec Equal { $$ = self.heap.apply_ref(Combinator::Not.into(), $2); }
    | e1 PlusPlus e1 { $$ = self.heap.apply2(Combinator::Append.into(), $1, $3); }
    | e1 PlusPlus { $$ = self.heap.apply_ref(Combinator::Append.into(), $1); }
    | e1 Colon e1 { $$ = self.heap.cons_ref($1, $3); }
    | e1 Colon { $$ = self.heap.apply_ref(Combinator::P.into(), $1); }
    | e1 MinusMinus e1 { $$ = self.heap.apply2(self.vm.listdiff_function(), $1, $3);  }
    | e1 MinusMinus { $$ = self.heap.apply_ref(self.vm.listdiff_function(), $1);  }
    | e1 Vel e1 { $$ = self.heap.apply2(Combinator::Or.into(), $1, $3); }
    | e1 Vel { $$ = self.heap.apply_ref(Combinator::Or.into(), $1); }
    | e1 Ampersand e1 { $$ = self.heap.apply2(Combinator::And.into(), $1, $3); }
    | e1 Ampersand { $$ = self.heap.apply_ref(Combinator::And.into(), $1); }
    | relsn { $$ = $1; }
    | es2 { $$ = $1; }
    ;

e2:
    Minus e2 %prec Minus { $$ = self.heap.apply_ref(Combinator::Neg.into(), $2); }
    | Hash e2 %prec Dot { $$ = self.heap.apply_ref(Combinator::Length.into(), $2);  }
    | e2 Plus e2 { $$ = self.heap.apply2(Combinator::Plus.into(), $1, $3); }
    | e2 Minus e2 { $$ = self.heap.apply2(Combinator::Minus.into(), $1, $3); }
    | e2 Times e2 { $$ = self.heap.apply2(Combinator::Times.into(), $1, $3); }
    | e2 Divide e2 { $$ = self.heap.apply2(Combinator::DivideFloat.into(), $1, $3); }
    | e2 IntegerDivide e2 { $$ = self.heap.apply2(Combinator::DivideInteger.into(), $1, $3); }
    | e2 Remainder e2 { $$ = self.heap.apply2(Combinator::Remainder.into(), $1, $3); }
    | e2 Caret e2 { $$ = self.heap.apply2(Combinator::Power.into(), $1, $3); }
    | e2 Dot e2 { $$ = self.heap.apply2(Combinator::B.into(), $1, $3);  }
    | e2 Bang e2 { $$ = self.heap.apply2(Combinator::Subscript.into(), $3, $1); }
    | e3 { $$ = $1; }
    ;

es2:               /* e2 or presection */
    Minus e2 %prec Minus { $$ = self.heap.apply_ref(Combinator::Neg.into(), $2); }
    | Hash e2 %prec Dot { $$ = self.heap.apply_ref(Combinator::Length.into(), $2);  }
    | e2 Plus e2 { $$ = self.heap.apply2(Combinator::Plus.into(), $1, $3); }
    | e2 Plus { $$ = self.heap.apply_ref(Combinator::Plus.into(), $1); }
    | e2 Minus e2 { $$ = self.heap.apply2(Combinator::Minus.into(), $1, $3); }
    | e2 Minus { $$ = self.heap.apply_ref(Combinator::Minus.into(), $1); }
    | e2 Times e2 { $$ = self.heap.apply2(Combinator::Times.into(), $1, $3); }
    | e2 Times { $$ = self.heap.apply_ref(Combinator::Times.into(), $1); }
    | e2 Divide e2 { $$ = self.heap.apply2(Combinator::DivideFloat.into(), $1, $3); }
    | e2 Divide { $$ = self.heap.apply_ref(Combinator::DivideFloat.into(), $1); }
    | e2 IntegerDivide e2 { $$ = self.heap.apply2(Combinator::DivideInteger.into(), $1, $3); }
    | e2 IntegerDivide { $$ = self.heap.apply_ref(Combinator::DivideInteger.into(), $1); }
    | e2 Remainder e2 { $$ = self.heap.apply2(Combinator::Remainder.into(), $1, $3); }
    | e2 Remainder { $$ = self.heap.apply_ref(Combinator::Remainder.into(), $1); }
    | e2 Caret e2 { $$ = self.heap.apply2(Combinator::Power.into(), $1, $3); }
    | e2 Caret { $$ = self.heap.apply_ref(Combinator::Power.into(), $1); }
    | e2 Dot e2 { $$ = self.heap.apply2(Combinator::B.into(), $1, $3);  }
    | e2 Dot { $$ = self.heap.apply_ref(Combinator::B.into(), $1);  }
    | e2 Bang e2 { $$ = self.heap.apply2(Combinator::Subscript.into(), $3, $1); }
    | e2 Bang { $$ = self.heap.apply2(Combinator::C.into(), Combinator::Subscript.into(), $1); }
    | es3 { $$ = $1; }
    ;

e3:
    comb InfixName e3 { $$ = self.heap.apply2($2, $1, $3); }
    | comb InfixCName e3 { $$ = self.heap.apply2($2, $1, $3); }
    | comb { $$ = $1; }
    ;

es3:                     /* e3 or presection */
    comb InfixName e3 { $$ = self.heap.apply2($2, $1, $3); }
    | comb InfixName { $$ = self.heap.apply_ref($2, $1); }
    | comb InfixCName e3 { $$ = self.heap.apply2($2, $1, $3); }
    | comb InfixCName { $$ = self.heap.apply_ref($2, $1); }
    | comb { $$ = $1; }
    ;

comb:
    comb arg { $$ = self.heap.apply_ref($1, $2); }
    | arg { $$ = $1; }
    ;

reln:
    e2 relop e2 { $$ = self.heap.apply2($2, $1, $3); }
    | reln relop e2 {
        /* EFFICIENCY PROBLEM - subject gets re-evaluated (and
            retypechecked) - fix later */
            let hd = self.heap[$1].head;
        let subject = if self.heap[hd].head == Combinator::And.into() {
          let reln_tail = self.heap[$1].tail;
          self.heap[reln_tail].tail
        } else {
          self.heap[$1].tail
        };
        let rhs = self.heap.apply2($2, subject.into(), $3);
        $$ = self.heap.apply2(Combinator::And.into(), $1, rhs);
      }
    ;

relsn:                     /* reln or presection */
    e2 relop e2 { $$ = self.heap.apply2($2, $1, $3); }
    | e2 relop { $$ = self.heap.apply_ref($2, $1); }
    | reln relop e2 {
        /* EFFICIENCY PROBLEM - subject gets re-evaluated (and
                    retypechecked) - fix later */

        let reln_head = self.heap[$1].head;
        let subject = if self.heap[reln_head].head == Combinator::And.into() {
          let reln_tail = self.heap[$1].tail;
          self.heap[reln_tail].tail
        } else {
          self.heap[$1].tail
        };
        let rhs = self.heap.apply2($2, subject.into(), $3);
        $$ = self.heap.apply2(Combinator::And.into(), $1, rhs);
      }
    ;

arg:
    /* `%lex` parsing remains outside the current expression-only parser path. */
    /* empty */ {
      if !self.syntax_error_found {
        self.lex_states = NIL_RAW;
        self.lex_mode = 1;
      }
      $$ = NIL;
    }
    Lex {
        self.syntax("%lex is not supported here\n");
        $$ = NIL;
      }
    | Name { $$ = $1; }
    | ConstructorName { $$ = $1; }
    | Constant { $$ = $1; }
    /* Interactive/session-only expression branches are not handled in the
       current parser path. */
    | ReadVals {
        self.syntax("interactive \"readvals\" is not supported here\n");
        $$ = NIL;
      }
    | Show {
        self.syntax("interactive \"show\" is not supported here\n");
        $$ = NIL;
      }
    | DollarDollar {
        self.syntax("interactive \"$$\" substitution is not supported here\n");
        $$ = NIL;
      }
    | OpenBracket CloseBracket { $$ = NIL; }
    | OpenBracket exp CloseBracket { $$ = self.heap.cons_ref($2, NIL); }
    | OpenBracket exp Comma exp CloseBracket {
        let rhs = self.heap.cons_ref($4, NIL);
        $$ = self.heap.cons_ref($2, rhs);
    }
    | OpenBracket exp Comma exp Comma liste CloseBracket {
        let reversed = ConsList::<Value>::reversed_value(self.heap, $6);
        let rhs = self.heap.cons_ref($4, reversed);
         $$ = self.heap.cons_ref($2, rhs);
     }
    | OpenBracket exp DotDot exp CloseBracket { $$ = self.heap.apply3(Combinator::StepUntil.into(), Value::Data(1), $4, $2); }
    | OpenBracket exp DotDot CloseBracket { $$ = self.heap.apply2(Combinator::Step.into(), Value::Data(1), $2); }
    | OpenBracket exp Comma exp DotDot exp CloseBracket {
        let minus_expr = self.heap.apply2(Combinator::Minus.into(), $4, $2);
        $$ = self.heap.apply3(Combinator::StepUntil.into(), minus_expr, $6, $2);
    }
    | OpenBracket exp Comma exp DotDot CloseBracket {
        let minus_expr= self.heap.apply2(Combinator::Minus.into(), $4, $2);
        $$ = self.heap.apply2(Combinator::Step.into(), minus_expr, $2);
     }
    /* Only list literals and ranges are handled here; list comprehensions are
       still unsupported. */
    | OpenBracket exp Pipe qualifiers CloseBracket {
        self.syntax("list comprehensions are not supported here\n");
        $$ = NIL;
      }
    | OpenBracket exp Diagonal qualifiers CloseBracket {
        self.syntax("diagonal list comprehensions are not supported here\n");
        $$ = NIL;
      }
    | OpenParenthesis op CloseParenthesis       /* RSB */ { $$ = $2; }
    | OpenParenthesis es1 CloseParenthesis      /* presection or parenthesised e1 */ { $$ = $2; }
    | OpenParenthesis diop1 e1 CloseParenthesis /* postsection */ {
        /* optimisation */
        let hd = self.heap[$2].head;
        $$ = if self.heap[$2].tag == Tag::Ap && hd == Combinator::C.into() {
          self.heap.apply_ref(self.heap[$2].tail.into(), $3)
        } else {
          self.heap.apply2(Combinator::C.into(), $2, $3)
        };
      }
    | OpenParenthesis CloseParenthesis {
        $$ = self.vm.void_tuple();
      }
    | OpenParenthesis exp Comma liste CloseParenthesis {
    	/*
    	A tuple. The heap representation of the tuple `(a1, ..., an)` is
        `self.heap.cons(a1, self.heap.cons(a2, ...pair(a(n-1), an)))`.
        */
        if self.heap[$4].tail == NIL_RAW {
          $$ = self.heap.pair_ref($2, self.heap[$4].head.into());
        } else {
          let tl = self.heap[$4].tail;
          $$ = self.heap.pair_ref(self.heap[tl].head.into(), self.heap[$4].head.into());
          let mut next_item = self.heap[tl].tail;
          while next_item != NIL_RAW {
            let hd = self.heap[next_item].head;
            $$ = self.heap.cons_ref(hd.into(), $$);
            next_item = self.heap[next_item].tail;
          }
          $$ = self.heap.cons_ref($2, $$);
        }

      }
    ;

lexrules:
    lexrules lstart here re indent { if !SYNERR { self.lex_mode = 2; } }

    Arrow exp lpostfix { if !SYNERR { self.lex_mode = 1; } } outdent {
        if $9 < 0 && e_re($4) {
          errs = $3;
          syntax("illegal lex rule - lhs matches empty\n");
        }
        let cons_a = self.heap.cons_ref($2, (1+$9).into());
        let cons_b = self.heap.cons_ref($4, self.heap.label_ref($3, $8));
        let cons = self.heap.cons_ref(cons_a, cons_a);
        $$ = self.heap.cons_ref(cons, $1);
      }
    | lex_rule_definitions { $$ = NIL; }
    ;

lstart:
    /* empty */ { $$ = RawValue::from(0).into(); }
    | Less cnames Greater {
        let mut ns = NIL;
        let mut next_item = $2;
    while next_item != NIL {
      let mut x = &mut self.lex_states;
      let mut i = 1;

      loop {
          if *x == NIL_RAW || self.heap[self.heap[*x].head].head == self.heap[next_item].head { break; }
          i += 1;
          x = &mut self.heap[*x].tail;
      }
      // Canonical `%lex` state records pair a constructor name with bit flags.
      // lstart sets the "appears in start-condition list" flag (bit 2) and
      // interns the corresponding numeric start-state index into `ns`.
      if *x == NIL_RAW {
          let hd = self.heap[next_item].head;
          let cons = self.heap.cons_ref(hd.into(), RawValue::from(2).into());
          *x = self.heap.cons_ref(cons, NIL).into();
      } else {
          let x_tail = self.heap[*x].tail;
          self.heap[x_tail].head |= 2;
      }
      ns = add1(i, ns);
      next_item = self.heap[next_item].tail.into()
    }
    $$ = ns;
      }
    ;

cnames:
    ConstructorName { $$ = self.heap.cons_ref($1, NIL); }
    | cnames ConstructorName {
        if member($1, $2) {
          println!(
            "{}syntax error: repeated name \"{}\" in start conditions\n",
            if echoing {"\n"} else {""},
            get_id($2)
          );
          acterror();
        }
        $$ = self.heap.cons_ref($2, $1);
      }
    ;

lpostfix:
        /* empty */ { $$ = RawValue::from(-1).into(); }
        | Begin ConstructorName
        {
          let mut x = &mut self.lex_states;
          let mut i = 1;
          loop
          {
        if *x == NIL_RAW { break; }
        let hd = self.heap[*x].head;
        if self.heap[hd].head == $2 { break; }
        i += 1;
        x = &mut self.heap[*x].tail;
          }
          if *x == NIL_RAW {
          let cons = self.heap.cons_ref($2, RawValue::from(1).into());
          *x = self.heap.cons_ref(cons, NIL).into();
          } else {
        let hd = self.heap[*x].head;
        self.heap[hd].tail |= 1;
          }
            $$ = RawValue::from(i).into();
          }
        | Begin Constant {
            if !isnat($2) || get_int($2)!=0 {
              syntax("%begin not followed by IDENTIFIER or 0\n");
            }
            $$ = RawValue::from(0).into();
          }
        ;

lex_rule_definitions:
    lex_rule_definitions LexDef indent Equal re outdent {
            let cons = self.heap.cons_ref($2, $5);
            self.lex_rule_definitions = self.heap.cons_ref(cons, self.lex_rule_definitions.into()).into();
        }
    | /* empty */ { self.lex_rule_definitions = NIL_RAW; }
    ;

re:   /* regular expression */
    re1 Pipe re { $$ = self.heap.apply2(Combinator::Lex_Or.into(), $1, $3); }
    | re1
    ;

re1:
    lterm Divide lterm { $$ = self.heap.apply2(Combinator::Lex_RContext.into(), $1, $3); }
    | lterm Divide { $$ = self.heap.apply2(Combinator::Lex_RContext.into(), $1, RawValue::from(0).into()); }
    | lterm
    ;

lterm:
    lfac lterm { $$ = self.heap.apply2(Combinator::Lex_Seq.into(), $1, $2); }
    | lfac
    ;

lfac:
    lunit Times {
        if (e_re($1)) {
          syntax("illegal regular expression - arg of * matches empty\n");
        }
        $$ = self.heap.apply_ref(Combinator::Lex_Star.into(), $1);
      }
    | lunit Plus { $$ = self.heap.apply2(Combinator::Lex_Seq.into(), $1, self.heap.apply_ref(Combinator::Lex_Star.into(), $1)); }
    | lunit QuestionMark { $$ = self.heap.apply_ref(Combinator::Lex_Opt.into(), $1); }
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
        $$ = if $1==Combinator::Nils.into() {
          self.heap.apply_ref(Combinator::Lex_String.into(), NIL)
        } else{
          if self.heap[$1].tail==NIL {
            let hd = self.heap[$1].head;
            self.heap.apply_ref(Combinator::Lex_Char.into(), hd)
          } else{
            self.heap.apply_ref(Combinator::Lex_String.into(), $1)
          }
        };
      }
    | CharClass {
        if($1==NIL){
          syntax("empty character class `` cannot match\n");
        }
        $$ = if self.heap[$1].tail==NIL {
          self.heap.apply_ref(Combinator::Lex_Char.into(), self.heap[$1].head)
        } else {
          self.heap.apply_ref(Combinator::Lex_Class.into(), $1)
        };
      }
    | AntiCharClass {
        let cons = self.heap.cons_ref(Token::AntiCharClass.into(), $1);
        $$ = self.heap.apply_ref(Combinator::Lex_Class.into(), cons);
      }
    | Dot { $$ = Combinator::Lex_Dot.into(); }
    | name {
        let mut x = self.lex_rule_definitions;
    loop {
        if x == NIL_RAW { break; }
        let hd = self.heap[x].head;
        if self.heap[hd].head == $1 { break; }
        x=tl[x];
    }
    if x == NIL_RAW {
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

/* List-comprehension qualifier/generator lowering is not implemented yet. */
qualifiers:
    exp {
        let guard = self.heap.cons_ref(GUARD, $1);
        $$ = self.heap.cons_ref(guard, NIL);
      }
    | generator { $$ = self.heap.cons_ref($1, NIL);  }
    | qualifiers Semicolon generator { $$ = self.heap.cons_ref($3, $1);   }
    | qualifiers Semicolon exp {
        let guard = self.heap.cons_ref(GUARD, $3);
        $$ = self.heap.cons_ref(guard, $1);
      }
    ;

generator:
    e1 Comma generator {
        self.syntax("generator syntax is not supported here\n");
        $$ = NIL;
      }
    | generator1
    ;

generator1:
    e1 LeftArrow exp {
        self.syntax("generator syntax is not supported here\n");
        $$ = NIL;
      }
    | e1 LeftArrow exp Comma exp DotDot {
        self.syntax("iterate-generator syntax is not supported here\n");
        $$ = NIL;
      }
    ;

/* Script-definition and type grammar continues below. */
defs:
    def
    | defs def
    ;

def:
    v act2 indent Equal here rhs outdent {
        let mut l = $1;
        let mut r = $6;
        let f = head(l);
        if self.heap[f].tag==Tag::Id && !isconstructor(f) {
          /* fnform defn */
          while self.heap[l].tag==Tag::Ap {
            r = lambda(self.heap[l].tail, r);
            l = self.heap[l].head;
          }
        }
        r = self.heap.label_ref($5, r);
        /* to help locate type errors */
        declare(l, r);
        self.last_identifier = l.into();
      }

    | spec {
        let mut h = reverse(self.heap[$1].head);
        let spec_tail = self.heap[$1].tail;
        let hr = self.heap[spec_tail].head;
        let t = self.heap[spec_tail].tail;
        while h!=NIL && !SYNERR {
          specify(self.heap[h].head, t, hr);
          h = self.heap[h].tail;
        }
        $$ = self.heap.cons_ref(NIL, Combinator::Nil);
      }

    | AbsoluteType here typeforms indent With lspecs outdent {
        // extern word TABSTRS;
        // extern char *dicp, *dicq;
        let mut x = reverse($6);
        let mut ids = NIL;
        let mut tids = NIL;
        while x!=NIL && !SYNERR {
          let hd_x = self.heap[x].head;
          let hd_hd_x = self.heap[hd_x].head;
          let hd_x_tail = self.heap[hd_x].tail;
          specify(
            hd_hd_x,
            self.heap.cons_ref(
              self.heap[hd_x_tail].tail,
              NIL
            ),
            self.heap[hd_x_tail].head
          );
          ids = self.heap.cons_ref(hd_hd_x, ids);
          x = self.heap[x].tail;
        }
        /* each id in specs has its id_type set to const(t, NIL) as a way
            of flagging that t is an abstract type */
        x = reverse($3);
        while x!=NIL && !SYNERR {
          decltype(
            self.heap[x].head,
            IdentifierValueTypeKind::Abstract,
            Type::Undefined.into(),
            $2
          );
          tids = self.heap.cons_ref(head(self.heap[x].head), tids);
          /* check for presence of showfunction */
          // Are identifier_dictionary and constructor_dictionary parts of a hash table?
          self.identifier_dictionary = "show".to_string() + get_id(self.heap[tids].head);

      // The original parser builds a temporary dictionary name of the form
      // `show<type>` and resolves it through `name()`. The typed identifier
      // lookup/intern seam for that path is still missing here.
           self.constructor_dictionary = String::new();
           let shfn = name();
           if member(ids, shfn) {
            t_showfn(self.heap[tids].head) = shfn;
          }
          x = self.heap[x].tail;
        }
        TABSTRS = self.heap.cons_ref(self.heap.cons_ref(tids, ids), TABSTRS);
        $$ = self.heap.cons_ref(self.heap.nill, NIL);
      }

    | typeform indent act1 here EqualEqual type act2 outdent {
        let x = redtvars(self.heap.apply_ref($1, $6).into());
        decltype(
          self.heap[x].head,
          IdentifierValueTypeKind::Synonym,
          self.heap[x].tail,
          $4
        );
        $$ = self.heap.cons_ref(self.heap.nill, NIL);
      }

    | typeform indent act1 here Colon2Equal construction act2 outdent {
        let rhs = $6;
        let mut r_ids = $6;
        let mut n = 0;

        while r_ids!=NIL {
          r_ids = self.heap[r_ids].tail;
          n += 1;
        }

        while rhs!=NIL && !SYNERR {
          let mut h = self.heap[rhs].head;
          let mut t = $1;
          let mut stricts = NIL;
          let mut i = 0;
          while self.heap[h].tag==Tag::Ap {
            if (
              self.heap[self.heap[h].tail].tag == Tag::Ap &&
              self.heap[self.heap[h].tail].head == Type::Strict.into()
            ) {
              stricts = self.heap.cons_ref(i.into(), stricts.into());
              self.heap[h].tail = tl[self.heap[h].tail];
            }
            t = self.heap.apply2(Type::Arrow.into(), self.heap[h].tail.into(), t);
            h = self.heap[h].head;
            i += 1;
          }
          if self.heap[h].tag == Tag::Id {
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
            let mut k = id_val(h);
            while (stricts!=NIL) {
              k = self.heap.apply2(Combinator::MkStrict.into(), (i - self.heap[stricts].head).into(), k);
              stricts = self.heap[stricts].tail;
            }
            id_val(h) = k; /* overwrite id_val of original constructor */
          }
          r_ids = self.heap.cons_ref(h, r_ids);
          rhs = self.heap[rhs].tail;
        }
        if (!SYNERR) {
          decltype($1, IdentifierValueTypeKind::Algebraic, r_ids, $4);
        }
        $$ = self.heap.cons_ref(self.heap.nill, NIL);
      }

    | indent setexp Export parts outdent {
        self.export_list_mode = false;
        if self.exported_identifiers != NIL_RAW {
          errs = $2;
          syntax("multiple %export statements are illegal\n");
        } else {
          if $4 == NIL && self.export_path_requests == NIL_RAW && self.export_embargoes != NIL_RAW {
            self.export_path_requests = self.heap.cons_ref(Combinator::Plus.into(), NIL).into();
          }
          self.exported_identifiers = self.heap.cons_ref($2, $4).into();  /* self.heap.cons(hereinfo, identifiers) */
        }
        $$ = self.heap.cons_ref(self.heap.nill, NIL);
      }

    | Free here OpenBrace specs CloseBrace {
        if self.free_identifiers != NIL_RAW {
          errs=$2;
          syntax("multiple %free statements are illegal\n");
        } else {
          let mut x = reverse($4);
          while (x!=NIL&&!SYNERR) {
            specify(hd[hd[x]], tl[tl[hd[x]]], hd[tl[hd[x]]]);
            self.free_identifiers = self.heap.cons_ref(head(hd[hd[x]]), self.free_identifiers.into()).into();
            if (tl[tl[hd[x]]]==Type::Type.into()) {
              t_class(self.heap[self.free_identifiers].head) = IdentifierValueTypeKind::Free;
            }
            else {
              id_val(self.heap[self.free_identifiers].head) = FREE; /* conventional value */
            }
            x = self.heap[x].tail;
          }
          fil_share(self.heap[files].head) = 0; /* parameterised scripts unshareable */
          self.free_identifiers = alfasort(self.free_identifiers.into()).into();
          let mut freeid_cursor = self.free_identifiers;
          while freeid_cursor!=NIL {
            /* each element of free_identifiers is of the form
              self.heap.cons(id, self.heap.cons(original_name, type)) */
            self.heap[freeid_cursor].head = self.heap.cons_ref(
              self.heap[freeid_cursor].head,
              self.heap.cons_ref(
                datapair(get_id(self.heap[freeid_cursor].head), 0),
                id_type(self.heap[freeid_cursor].head)
              )
            );
            freeid_cursor = self.heap[freeid_cursor].tail;
          }
        }
        $$ = self.heap.cons_ref(self.heap.nill, NIL);
      }

    | Include bindings modifiers outdent { /* fiddle - 'indent' done by yylex() on reading fileid */
        // extern char *dicp;
        // extern word CLASHES, BAD_DUMP;
        /* $1 contains file+hereinfo */
        self.include_requests = self.heap.cons_ref(self.heap.cons_ref($1, self.heap.cons_ref($3, $2)), self.include_requests.into()).into();
        $$ = self.heap.cons_ref(self.heap.nill, NIL);
      }

    | here BNF { startbnf(); self.bnf_mode = 1; } names outdent productions EndIR {
        /* fiddle - `indent' done by yylex() while processing directive */
        let mut lhs = NIL;
        let mut p = $6;
        let mut subjects = NIL;
        let mut body = NIL;
        let mut startswith = NIL;
        let mut leftrecs = NIL;
        self.inherited_attributes = 0;
        self.bnf_mode = 0;
        self.nonterminals = UNION(self.nonterminals.into(), $4).into();
        while p!=NIL {
          if (dval(self.heap[p].head)==Combinator::Undef.into()) {
            self.nonterminals = add1(dlhs(self.heap[p].head), self.nonterminals.into()).into();
          }
          else{
            lhs = add1(dlhs(self.heap[p].head), lhs);
          }
          p = self.heap[p].tail;
        }
        self.nonterminals = setdiff(self.nonterminals.into(), lhs).into();
        if self.nonterminals != NIL_RAW {
          errs = $1;
          member($4, self.heap[self.nonterminals].head); /*||findnt(self.heap[self.nonterminals].head)*/
          printf("%sfatal error in grammar, ", if echoing {"\n"} else {""});
          printf(
            "undefined nonterminal%s: ",
            if self.heap[self.nonterminals].tail==NIL_RAW {""} else {"s"}
          );
          printlist("", self.nonterminals.into());
          acterror();
        } else { /* compute list of nonterminals admitting empty prodn */
          self.empty_production_nonterminals = NIL_RAW;
          let mut changed = true;
          while changed {
            changed = false;
            p = $6;
            while p!=NIL {
              if(!member(self.empty_production_nonterminals.into(), dlhs(self.heap[p].head)) && eprod(dval(self.heap[p].head))) {
                self.empty_production_nonterminals = self.heap.cons_ref(dlhs(self.heap[p].head), self.empty_production_nonterminals.into()).into();
                changed = true;
              }
              p = self.heap[p].tail;
            }
          }
          /*
            now compute startswith reln between nonterminals
            (performing binomial transformation en route)
            and use to detect unremoved left recursion
          */
          p = $6;
          while p!=NIL {
            lhs = starts(dval(self.heap[p].head));
            if (member(lhs, dlhs(self.heap[p].head))) {
              binom(dval(self.heap[p].head), dlhs(self.heap[p].head));
              startswith = self.heap.cons_ref(
                self.heap.cons_ref(
                  dlhs(self.heap[p].head),
                  starts(dval(self.heap[p].head))
                ),
                startswith
              );
            } else {
              startswith = self.heap.cons_ref(self.heap.cons_ref(dlhs(self.heap[p].head), lhs), startswith);
            }
            p = self.heap[p].tail;
          }
          startswith = tclos(sortrel(startswith));
          while startswith!=NIL {
            if (member(tl[hd[startswith]], hd[hd[startswith]])) {
              leftrecs = add1(hd[hd[startswith]], leftrecs);
            }
            startswith = self.heap[startswith].tail;
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
          let mut start_symbols = $4;
          if(start_symbols==NIL) { /* implied start symbol */
            start_symbols = self.heap.cons_ref(dlhs(self.heap[lastlink($6)].head), NIL);
          }
          self.bnf_enabled = 1; /* `%bnf` is enabled for this parser run. */
          if (self.heap[start_symbols].tail==NIL) { /* only one start symbol */
            subjects = getfname(self.heap[start_symbols].head);
            body = self.heap.apply2(Combinator::G_Close.into(), str_conv(get_id(self.heap[start_symbols].head)), self.heap[start_symbols].head);
          } else {
            body = Void;
            subjects = Void;
            while start_symbols!=NIL {
              subjects = self.heap.pair_ref(getfname(self.heap[start_symbols].head), subjects);
              body = self.heap.pair_ref(
                self.heap.apply2(Combinator::G_Close.into(), str_conv(get_id(self.heap[start_symbols].head)), self.heap[start_symbols].head),
                body
              );
              start_symbols = self.heap[start_symbols].tail;
            }
          }
          declare(subjects, self.heap.label_ref($1, block($6, body, 0)));
        }
      }
    ;

setexp:
    here {
        /* hack to fix lex analyser */
        $$ = $1;
        self.export_list_mode = true;
      }
    ;

bindings:
    /* empty */ { $$ = NIL; }
    | OpenBrace bindingseq CloseBrace { $$ = $2; }
    ;

bindingseq:
    bindingseq binding { $$ = self.heap.cons_ref($2, $1); }
    | binding { $$ = self.heap.cons_ref($1, NIL); }
    ;

binding:
    Name indent Equal exp outdent { $$ = self.heap.cons_ref($1, $4); }
    | typeform indent act1 EqualEqual type act2 outdent {
        let x = redtvars(self.heap.apply_ref($1, $5).into());
        let mut arity = 0;
        let mut h = self.heap[x].head;
        while self.heap[h].tag == Tag::Ap {
          arity += 1;
          h = self.heap[h].head;
        }
        $$ = self.heap.apply_ref(
          h.into(),
          make_typ(arity, 0, IdentifierValueTypeKind::Synonym, self.heap[x].tail).into()
        ).into();
      }
    ;

modifiers:
    /* empty */ { $$ = NIL; }
    | negmods {
        let mut a = $1;
        let mut c = NIL;
        while a!=NIL {
          let alias_a = self.heap[a].head;
          let left_a = self.heap[alias_a].head;
          let right_a = self.heap[alias_a].tail;
          let mut b = self.heap[a].tail;
          while b!=NIL {
            let alias_b = self.heap[b].head;
            if(left_a==self.heap[alias_b].head) {
              c = left_a.into();
            }
            if (right_a==self.heap[alias_b].tail) {
              c = right_a.into();
            }
            if(c!=NIL){
              break;
            }
            b = self.heap[b].tail;
          }
          if(c!=NIL) {
            break;
          }
          a = self.heap[a].tail;
        }
        if(c!=NIL) {
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
    negmods negmod { $$ = self.heap.cons_ref($2, $1); }
    | negmod { $$ = self.heap.cons_ref($1, NIL); }
    ;

negmod:
    Name Divide Name { $$ = self.heap.cons_ref($1, $3); }
    | ConstructorName Divide ConstructorName { $$ = self.heap.cons_ref($1, $3); }
    | Minus Name { $$ = self.heap.cons_ref(make_pn(Combinator::Undef.into()).into(), $2); }
    /*| Minus ConstructorName  no - cannot suppress constructors selectively */
    ;

here:
    /* empty */ {
        // extern word line_no;
        self.last_diagnostic_location = fileinfo(get_fil(current_file), line_no).into();
        $$ = self.last_diagnostic_location.into();
        /* (script, line_no) for diagnostics */
      }
    ;

act1:
    /* empty */ { self.type_variable_scope = true; $$ = NIL; };

act2:
    /* empty */ {
        self.type_variable_scope = false;
        self.used_identifiers = NIL_RAW;
        $$ = NIL;
      }
    ;

ldefs:
    ldef {
        $$ = self.heap.cons_ref($1, NIL);
        dval($1) = self.heap.tries_ref(dlhs($1), self.heap.cons_ref(dval($1), NIL));
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
          self.heap[dval(self.heap[$1].head.tail)] = self.heap.cons_ref(dval($2), self.heap[dval(self.heap[$1].head.tail)]);
        } else if (!SYNERR) {
          let ns = get_ids(dlhs($2));
          let hr = self.heap[dval($2)].head;
          if(ns==NIL){
            errs = hr;
            syntax("illegal lhs for local definition\n");
          }
          $$ = self.heap.cons_ref($2, $1);
          dval($2) = self.heap.tries_ref(dlhs($2), self.heap.cons_ref(dval($2), NIL));
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
        $$ = self.heap.cons_ref(self.heap.nill, NIL);
      }
    | typeform here EqualEqual {
        errs=$2;
        syntax("`==' encountered in local defs\n");
        $$ = self.heap.cons_ref(self.heap.nill, NIL);
      }
    | typeform here Colon2Equal {
        errs=$2;
        syntax("`::=' encountered in local defs\n");
        $$ = self.heap.cons_ref(self.heap.nill, NIL);
      }
    | v act2 indent Equal here rhs outdent {
        let l = $1;
        let r = $6;
        let f = self.heap[l].head;
        if(self.heap[f].tag == Tag::Id && !isconstructor(f)) { /* fnform defn */
          while(self.heap[l].tag == Tag::Ap){
            r = lambda(self.heap[l].tail, r);
            l = self.heap[l].head;
          }
        }
        r = self.heap.label_ref($5, r); /* to help locate type errors */
        $$ = defn(l, Type::Undefined.into(), r);
      }
    ;

vlist:
    v { $$ = self.heap.cons_ref($1, NIL); }
    | vlist Comma v /* left recursive so as not to eat YACC stack */ {
        /* reverse order, NB */
        $$ = self.heap.cons_ref($3, $1);
      }
    ;

v:
    v1
    | v1 Colon v { $$ = self.heap.cons_ref($1, $3); }
    ;

v1:
    v1 Plus Constant  /* n+k pattern */ {
        if (!isnat($3)){
          syntax("inappropriate use of \"+\" in pattern\n");
        }
        $$ = self.heap.apply2(Combinator::Plus.into(), $3, $1);
      }
    | Minus Constant {
        /* if(tag[$2]==DOUBLE)
              $$ = self.heap.cons(Constant, sto_dbl(-get_dbl($2))); else */
        if (tag[$2]==Tag::Int) {
          $$ = self.heap.cons_ref(Token::Constant.into(), bignegate($2));
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
        $$ = self.heap.apply_ref(
          if self.heap[$1].head==Token::Constant.into() && tag[tl[$1]]==Tag::Id {
            self.heap[$1].tail.into()
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
        if (self.semantic_reduction_count != 0 && member(gvars, $1)){
          syntax("illegal use of $num symbol\n");
        }
        /* cannot use grammar variable in a binding position */
        if (memb(self.used_identifiers.into(), $1)) {
          $$ = self.heap.cons_ref(Token::Constant.into(), $1);
        } /* picks up repeated names in a template */
        else {
          self.used_identifiers = self.heap.cons_ref($1, self.used_identifiers.into()).into();
        }
      }
    | ConstructorName
    | Constant {
        if (tag[$1]==Tag::Double) {
          syntax("use of floating point literal in pattern\n");
        }
        $$ = self.heap.cons_ref(Token::Constant.into(), $1);
      }
    | OpenBracket CloseBracket { $$ = self.heap.nill; }
    | OpenBracket vlist CloseBracket {
        let mut x = $2;
        let mut y = self.heap.nill;
        while(x!=NIL) {
          y = self.heap.cons_ref(self.heap[x].head, y);
          x = self.heap[x].tail;
        }
        $$ = y;
      }
    | OpenParenthesis CloseParenthesis { $$ = Void; }
    | OpenParenthesis v CloseParenthesis { $$ = $2; }
    | OpenParenthesis v Comma vlist CloseParenthesis {
        /* representation of the tuple (a1, ..., an) is
             self.heap.cons_ref(a1, self.heap.cons_ref(a2, ...pair(a(n-1), an))) */
        if(self.heap[$4].tail==NIL) {
          $$ = self.heap.pair_ref($2, self.heap[$4].head.into());
        } else {
          let second_pair = self.heap[$4].tail;
          $$ = self.heap.pair_ref(self.heap[second_pair].head.into(), self.heap[$4].head.into());
          let mut rest = self.heap[second_pair].tail;
          while (rest!=NIL) {
            $$ = self.heap.cons_ref(self.heap[rest].head.into(), $$);
            rest = self.heap[rest].tail;
          }
          $$ = self.heap.cons_ref($2, $$);
        }

      }
    ;

type:
    type1
    | type Arrow type { $$ = self.heap.apply2(Type::Arrow.into(), $1, $3); }
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
    Name argtype { $$ = self.heap.apply_ref($1, $2); }
    | tap argtype { $$ = self.heap.apply_ref($1, $2); }
    ;

argtype:
    Name { $$ = transtypeid($1); }
           /* necessary while prelude not meta_tchecked (for prelude) */
    | typevar {
        if (self.type_variable_scope && !memb(self.used_identifiers.into(), $1)){
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
    | OpenBracket type CloseBracket  /* at release one was `typelist' */ { $$ = self.heap.apply_ref(Type::List.into(), $2); }
    | OpenBracket type Comma typel CloseBracket {
        syntax(
          "tuple-type with missing parentheses (obsolete syntax)\n"
        );
      }
    ;

typelist:
    /* empty */ { $$ = Type::Void.into(); }  /* voidtype */
    | type
    | type Comma typel {
        let mut x = $3;
        let mut y = Type::Void.into();
        while (x!=NIL) {
          y = self.heap.apply2(Type::Comma.into(), self.heap[x].head.into(), y);
          x = self.heap[x].tail;
        }
        $$ = self.heap.apply2(Type::Comma.into(), $1, y);
      }
    ;

typel:
    type { $$ = self.heap.cons_ref($1, NIL); }
    | typel Comma type /* left recursive so as not to eat YACC stack */ { $$ = self.heap.cons_ref($3, $1); }
    ;

parts: /* returned in reverse order */
    parts Name { $$ = add1($2, $1); }
    | parts Minus Name {
        $$ = $1;
        self.export_embargoes = add1($3, self.export_embargoes.into()).into();
      }
    | parts PathName { $$ = $1; } /* the pathnames are placed on export_path_requests in yylex */
    | parts Plus {
        $$ = $1;
        self.export_path_requests = self.heap.cons_ref(Combinator::Plus.into(), self.export_path_requests.into()).into();
      }
    | Name { $$ = add1($1, NIL); }
    | Minus Name {
        $$ = NIL;
        self.export_embargoes = add1($2, self.export_embargoes.into()).into();
      }
    | PathName { $$ = NIL; }
    | Plus {
        $$ = NIL;
        self.export_path_requests = self.heap.cons_ref(Combinator::Plus.into(), self.export_path_requests.into()).into();
      }
    ;

specs:  /* returns a list of self.heap.cons(id, self.heap.cons(here, type))
           in reverse order of appearance */
    specs spec {
        let mut x = $1;
        let mut h = self.heap[$2].head;
        let t = self.heap[$2].tail;
        while (h!=NIL) {
          x = self.heap.cons_ref(self.heap.cons_ref(self.heap[h].head, t), x);
          h = self.heap[h].tail;
        }
        $$ = x;
      }
    | spec {
        let mut x = NIL;
        let mut h = self.heap[$1].head;
        let t = self.heap[$1].tail;
        while (h!=NIL) {
          x = self.heap.cons_ref(self.heap.cons_ref(self.heap[h].head, t), x);
          h = self.heap[h].tail;
        }
        $$ = x;
      }
    ;

spec:
    typeforms indent here ColonColon ttype outdent {
        $$ = self.heap.cons_ref($1, self.heap.cons_ref($3, $5));
      } /* hack: `typeforms' includes `namelist' */
    ;

lspecs:  /* returns a list of self.heap.cons(id, self.heap.cons(here, type))
           in reverse order of appearance */
    lspecs lspec {
        let mut x = $1;
        let mut h = self.heap[$2].head;
        let t = self.heap[$2].tail;
        while(h!=NIL) {
          x = self.heap.cons_ref(self.heap.cons_ref(self.heap[h].head, t), x);
          h = self.heap[h].tail;
        }
        $$ = x;
      }
    | lspec {
        let mut x = NIL;
        let mut h = self.heap[$1].head;
        let t = self.heap[$1].tail;
        while(h!=NIL) {
          x = self.heap.cons_ref(self.heap.cons_ref(self.heap[h].head, t), x);
          h = self.heap[h].tail;
        }
        $$ = x;
      }
    ;

lspec:
    namelist indent here {self.bnf_mode=0;} ColonColon type outdent { $$ = self.heap.cons_ref($1, self.heap.cons_ref($3, $6)); };

namelist:
    Name Comma namelist { $$ = self.heap.cons_ref($1, $3); }
    | Name { $$ = self.heap.cons_ref($1, NIL); }
    ;

typeforms:
    typeforms Comma typeform act2 { $$ = self.heap.cons_ref($3, $1); }
    | typeform act2 { $$ = self.heap.cons_ref($1, NIL); }
    ;

typeform:
    ConstructorName typevars { syntax("upper case identifier out of context\n"); }
    | Name typevars   /* warning if typevar is repeated */ {
        $$ = $1;
        self.used_identifiers = $2.into();
        let mut typevars = $2;
        while(typevars!=NIL) {
          $$ = self.heap.apply_ref($$, self.heap[typevars].head.into());
          typevars = self.heap[typevars].tail;
        }
      }
    | typevar InfixName typevar {
        if(eqtvar($1, $3)){
          syntax("repeated type variable in typeform\n");
        }
        self.used_identifiers = self.heap.cons_ref($1, self.heap.cons_ref($3, NIL)).into();
        $$ = self.heap.apply2($2, $1, $3);
      }
    | typevar InfixCName typevar {
        syntax("upper case identifier cannot be used as typename\n");
      }
    ;

ttype:
    type
    | Type { $$ = Type::Type.into(); }
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
        $$ = self.heap.cons_ref($1, $2);
      }
    ;

construction:
    constructs { /* keeps track of sui-generis constructors */
        // extern word SGC;
        if ( self.heap[$1].tail==NIL && tag[hd[$1]]!=Tag::Id ) {
                        /* 2nd conjunct excludes singularity types */
          SGC = self.heap.cons_ref(head(self.heap[$1].head), SGC);
        }
      }
    ;

constructs:
    construct { $$ = self.heap.cons_ref($1, NIL); }
    | constructs Pipe construct { $$ = self.heap.cons_ref($3, $1); }
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
    | construct1 field1 { $$ = self.heap.apply_ref($1, $2); }
    | here ConstructorName {
        $$ = $2;
        id_who($2) = $1;
      }
    ;

field:
    type
    | argtype Bang { $$ = self.heap.apply_ref(Type::Strict.into(), $1); }
    ;

field1:
    argtype Bang { $$ = self.heap.apply_ref(Type::Strict.into(), $1); }
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
            if self.bnf_mode != 0 {"bnf"} else {"attribute"}
          );
          acterror();
        }
        $$ = if self.bnf_mode != 0 {
          add1($2, $1)
        } else {
          self.heap.cons_ref($2, $1)
        };
      }
    ;

/* `%bnf` grammar-analysis and lowering is still incomplete here. */
productions:
    lspec {
        let mut h = reverse(self.heap[$1].head);
        let hr = hd[tl[$1]];
        let t = tl[tl[$1]];
        self.bnf_mode = 1;
        $$ = NIL;
        while (h!=NIL && !SYNERR) {
          self.nonterminal_specification_map = self.heap.cons_ref(self.heap.cons_ref(self.heap[h].head, hr), self.nonterminal_specification_map.into()).into();
          $$ = add_prod(defn(self.heap[h].head, t, Combinator::Undef.into()), $$, hr);
          h = self.heap[h].tail;
        }
      }
    | production { $$ = self.heap.cons_ref($1, NIL); }
    | productions lspec {
        let mut h = reverse(self.heap[$2].head);
        let hr = hd[tl[$2]];
        let t = tl[tl[$2]];
        self.bnf_mode = 1;
        $$=$1;
        while(h!=NIL&&!SYNERR){
          self.nonterminal_specification_map = self.heap.cons_ref(self.heap.cons_ref(self.heap[h].head, hr), self.nonterminal_specification_map.into()).into();
          $$ = add_prod(defn(self.heap[h].head, t, Combinator::Undef.into()), $$, hr);
          h = self.heap[h].tail;
        }
      }
    | productions production { $$ = add_prod($2, $1, self.heap[dval($2)].head); }
    ;

production:
    Name params Colon indent grhs outdent
      /* found by experiment that indent must follow Colon here */ {
        $$ = defn($1, Type::Undefined.into(), $5);
      }
    ;

params:   /* places inherited attributes, if any, on inherited_attributes */
    /* empty */ { self.inherited_attributes=0; $$ = NIL; }
    | { self.bnf_mode=0; } OpenParenthesis names CloseParenthesis {
        self.bnf_mode = 1;
        if ($3==NIL) {
          syntax("unexpected token CloseParenthesis\n");
        }
        self.inherited_attributes = $3.into();
      }
    ;

grhs:
    here phrase { $$ = self.heap.label_ref($1, $2); }
    ;

phrase:
    error_term { $$ = self.heap.apply2(Combinator::G_Error.into(), Combinator::G_Zero.into(), $1); }
    | phrase1 {
        $$ = self.heap[$1].head.into();
        let mut branches = self.heap[$1].tail;
        while (branches!=NIL) {
          $$ = self.heap.label_ref(self.heap[branches].head.into(), $$);
          branches = self.heap[branches].tail;
          $$ = self.heap.apply2(Combinator::G_Alt.into(), self.heap[branches].head.into(), $$);
          branches = self.heap[branches].tail;
        }
      }
    | phrase1 Pipe error_term {
        $$ = self.heap[$1].head.into();
        let mut branches = self.heap[$1].tail;
        while(branches!=NIL){
          $$ = self.heap.label_ref(self.heap[branches].head.into(), $$);
          branches = self.heap[branches].tail;
          $$ = self.heap.apply2(Combinator::G_Alt.into(), self.heap[branches].head.into(), $$);
          branches = self.heap[branches].tail;
        }
        $$ = self.heap.apply2(Combinator::G_Error.into(), $$, $3);
      }
      /* we right rotate G_ALT's to facilitate left factoring (see trans) */
    ;

phrase1:
    term { $$=self.heap.cons_ref($1, NIL); }
    | phrase1 Pipe here term { $$ = self.heap.cons_ref($4, self.heap.cons_ref($3, $1)); }
    ;

term:
    count_factors {
        syntax("%bnf production lowering is not implemented here\n");
        $$ = NIL;
      }
    | count_factors {self.bnf_mode=2;} indent Equal here rhs outdent {
        syntax("%bnf production lowering is not implemented here\n");
        self.bnf_mode = 1;
        self.semantic_reduction_count = 0;
        $$ = NIL;
      }
    ;

error_term:
    ErrorSymbol {
        syntax("%bnf error productions are not implemented here\n");
        $$ = NIL;
      }
    | ErrorSymbol {
        self.bnf_mode = 2;
        self.semantic_reduction_count = 2;
      } indent Equal here rhs outdent {
        syntax("%bnf error productions are not implemented here\n");
        self.bnf_mode = 1;
        self.semantic_reduction_count = 0;
        $$ = NIL;
      }
    ;

count_factors:
    EmptySymbol {
        self.semantic_reduction_count = 0;
        $$ = NIL;
      }
    | EmptySymbol factors {
        syntax("unexpected token after empty\n");
        self.semantic_reduction_count = 0;
        $$ = NIL;
        }
    | { self.open_bracket_count=0; } factors {
        let mut f = $2;
        if self.open_bracket_count != 0 {
          syntax(if self.open_bracket_count > 0 {
            "unmatched { in grammar rule\n"
          } else {
            "unmatched } in grammar rule\n"
          });
        }
        self.semantic_reduction_count = 0;
        while f != NIL {
          self.semantic_reduction_count += 1;
          f = self.heap[f].tail.into();
        }
        if self.heap[$2].head == Combinator::G_End.into() {
          self.semantic_reduction_count -= 1;
        }
        $$ = $2;
      }
    ;

factors:
    factor { $$ = self.heap.cons_ref($1, NIL); }
    | factors factor {
        if (self.heap[$1].head==Combinator::G_End.into()) {
          syntax("unexpected token after end\n");
        }
        $$ = self.heap.cons_ref($2, $1);
      }
    ;

factor:
    unit
    | OpenBrace unit CloseBrace {
        $$ = self.heap.apply_ref(outdent_fn.into(), self.heap.apply2(indent_fn, getcol_fn(), $2).into());
      }
    | OpenBrace unit {
        self.open_bracket_count += 1;
        $$ = self.heap.apply2(indent_fn, getcol_fn(), $2);
      }
    | unit CloseBrace {
        self.open_bracket_count -= 1;
          if self.open_bracket_count < 0 {
            syntax("unmatched `}' in grammar rule\n");
          }
        $$ = self.heap.apply_ref(outdent_fn.into(), $1);
      }
    ;

unit:
    symbol
    | symbol Times { $$ = self.heap.apply_ref(Combinator::G_Star.into(), $1); }
    | symbol Plus {
        $$ = self.heap.apply2(
                    Combinator::G_Seq.into(),
                    $1,
                    self.heap.apply2(
                      Combinator::G_Seq.into(),
                      self.heap.apply_ref(
                        Combinator::G_Star.into(),
                        $1
                      ),
                      self.heap.apply_ref(
                        Combinator::G_Rule.into(),
                        self.heap.apply_ref(
                          Combinator::C.into(),
                          Combinator::P.into()
                        )
                      )
                    )
                  );
      }
    | symbol QuestionMark { $$ = self.heap.apply_ref(Combinator::G_Opt.into(), $1); }
    ;

symbol:
    Name {
        // The original `%bnf` path records the symbol in `nonterminals` and
        // conditionally appends to `nonterminal_map` through the `NEW` side channel.
        // That bookkeeping is still not wired here.
        $$ = $1;
      }
    | EndSymbol { $$ = Combinator::G_End.into(); }
    | Constant {
        if(!isstring($1)) {
          print!(
            "{}syntax error: illegal terminal ",
            if echoing {"\n" }else {""}
          );
          out(stdout, $1);
          print!(" (should be string-const)\n");
          acterror();
        }
        $$ = self.heap.apply_ref(Combinator::G_Symb.into(), $1);
      }
    | Caret { $$=Combinator::G_State.into(); }
    | {self.bnf_mode=0;} OpenBracket exp {self.bnf_mode=1;} CloseBracket { $$ = self.heap.apply_ref(Combinator::G_SuchThat.into(), $3); }
    | Minus { $$ = Combinator::G_Any.into(); }
    ;

%%
/*  end of Miranda rules  */



impl<'ctx /* 'fix quotes */> Parser<'ctx /* 'fix quotes */> {
    pub fn new(lexer: Lexer, activation: ParserActivation<'ctx /* 'fix quotes */>) -> Self {
      let ParserActivation {
        heap,
        vm,
        session,
        deferred,
      } = activation;

      Self {
        yy_error_verbose: true,
        yynerrs: 0,
        yyerrstatus_: 0,
        result: None,
        debug: false,
        yylexer: lexer,

        heap,
        vm,
        syntax_error_found: false,
        diagnostics: ParserRunDiagnostics::default(),
        top_level_script_parsed: false,
        directive_include_requests: Vec::new(),
        definition_payloads: Vec::new(),
        specification_payloads: Vec::new(),
        type_declaration_payloads: Vec::new(),
        constructor_payloads: Vec::new(),
        free_binding_payloads: Vec::new(),

        lex_states: session.lex_states,
        used_identifiers: session.used_identifiers,
        lex_rule_definitions: session.lex_rule_definitions,
        last_identifier: session.last_identifier,
        inherited_attributes: session.inherited_attributes,
        nonterminals: session.nonterminals,
        empty_production_nonterminals: session.empty_production_nonterminals,
        nonterminal_specification_map: session.nonterminal_specification_map,
        nonterminal_map: session.nonterminal_map,
        last_diagnostic_location: session.last_diagnostic_location,

        bnf_mode: session.bnf_mode,
        export_list_mode: session.export_list_mode,
        lex_mode: session.lex_mode,
        type_variable_scope: session.type_variable_scope,
        semantic_reduction_count: session.semantic_reduction_count,
        open_bracket_count: session.open_bracket_count,

        exported_identifiers: deferred.exported_identifiers,
        export_path_requests: deferred.export_path_requests,
        export_embargoes: deferred.export_embargoes,
        include_requests: deferred.include_requests,
        free_identifiers: deferred.free_identifiers,

        bnf_enabled: 0,
        identifier_dictionary: String::new(),
        constructor_dictionary: String::new(),
      }
    }


    fn next_token(&mut self) -> ParserLookahead {
      match self.yylexer.yylex() {
        Ok(mut lookahead) => {
          let source_text = self.yylexer.source_text();
          let start = (lookahead.loc.begin as usize).min(source_text.len());
          let end = (lookahead.loc.end as usize).min(source_text.len());
          let source_slice = &source_text[start..end];

          match lookahead.token {
            Token::Identifier => {
              let identifier = self.heap
                .get_identifier(source_slice)
                .unwrap_or_else(|| self.heap.make_empty_identifier(source_slice));
              lookahead.token = Token::Name;
              lookahead.token_type = Token::Name as i32 + 2;
              lookahead.value = identifier.into();
            }
            Token::ConstructorName => {
              let identifier = self.heap
                .get_identifier(source_slice)
                .unwrap_or_else(|| self.heap.make_empty_identifier(source_slice));
              lookahead.value = identifier.into();
            }
            Token::InfixName | Token::InfixCName => {
              let identifier_name = source_slice.strip_prefix('$').unwrap_or(source_slice);
              let identifier = self.heap
                .get_identifier(identifier_name)
                .unwrap_or_else(|| self.heap.make_empty_identifier(identifier_name));
              lookahead.value = identifier.into();
            }
            Token::Integer => {
              if let Ok(integer) = source_slice.parse::<i64>() {
                lookahead.token = Token::Constant;
                lookahead.token_type = Token::Constant as i32 + 2;
                lookahead.value = IntegerRef::from_i64(self.heap, integer).into();
              }
            }
            Token::Float => {
              if let Ok(real) = source_slice.parse::<f64>() {
                lookahead.token = Token::Constant;
                lookahead.token_type = Token::Constant as i32 + 2;
                lookahead.value = self.heap.real_ref(real);
              }
            }
            Token::StringLiteral => {
              let string_contents = source_slice
                .strip_prefix('"')
                .and_then(|slice| slice.strip_suffix('"'))
                .unwrap_or(source_slice);
              lookahead.value = Value::Reference(self.heap.string(string_contents));
            }
            _ => {}
          }

          lookahead
        }
        Err(error) => {
          eprintln!("Lexer error: {}", error);
          panic!();
          // return Ok(Self::YYABORT);
        }
      }
    }

    fn syntax(&mut self, message: &str) {
      let location = Some(self.yylexer.current_loc());
      let here_info = Some(HereInfo::from_source_location(
        self.yylexer.source_name(),
        self.yylexer.source_text(),
        location,
      ));
      self.diagnostics.push(ParserDiagnostic {
        message: message.trim_end().to_string(),
        location,
        here_info,
      });
      self.syntax_error_found = true;
    }
    

    fn report_syntax_error(&mut self, _yystack: &YYStack, yytoken: &SymbolKind, yylloc: YYLoc) {
        let token_name = SymbolKind::yynames_[i32_to_usize(yytoken.code())];
        let here_info = Some(HereInfo::from_source_location(
          self.yylexer.source_name(),
          self.yylexer.source_text(),
          Some(yylloc),
        ));
        self.diagnostics.push(ParserDiagnostic {
          message: format!("unexpected token {}", token_name),
          location: Some(yylloc),
          here_info,
        });
        self.syntax_error_found = true;
    }

    pub fn finish_run(self, parse_ok: bool) -> ParserRunResult {
        if self.syntax_error_found || !parse_ok {
            return ParserRunResult::SyntaxError(self.diagnostics);
        }

        if self.top_level_script_parsed {
            let export = if self.exported_identifiers == NIL_RAW
                && self.export_path_requests == NIL_RAW
                && self.export_embargoes == NIL_RAW
            {
                None
            } else {
                let (anchor, exported_ids) = if self.exported_identifiers == NIL_RAW {
                    (NIL_RAW, NIL_RAW)
                } else {
                    (
                        self.heap[self.exported_identifiers].head,
                        self.heap[self.exported_identifiers].tail,
                    )
                };

                Some(ParserExportDirectivePayload {
                    anchor,
                    exported_ids,
                    pathname_requests: self.export_path_requests,
                    embargoes: self.export_embargoes,
                })
            };

            return ParserRunResult::ParsedTopLevelScript(ParserTopLevelScriptPayload {
                directives: ParserTopLevelDirectivePayload {
                    include_requests: self.directive_include_requests,
                    export,
                },
                definitions: self.definition_payloads,
                specifications: self.specification_payloads,
                type_declarations: self.type_declaration_payloads,
                constructor_declarations: self.constructor_payloads,
                free_bindings: self.free_binding_payloads,
            });
        }

        match self.result {
            Some(result) => ParserRunResult::ParsedExpression(result),
            None => ParserRunResult::SyntaxError(self.diagnostics),
        }
    }

}
