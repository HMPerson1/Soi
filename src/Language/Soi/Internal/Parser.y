{
module Language.Soi.Internal.Parser
  ( parseFileP
  ) where

import           ClassyPrelude

import           Language.Soi.Internal.Lexer
import           Language.Soi.Internal.ParseMonad
}

%expect 4
{-
--------------------------------------------------------------------------------

state 42 contains 1 shift/reduce conflicts.

	  *** l_value -> l_value0 .
	      l_value0 -> l_value0 . '.' ID_VAR

    Conflicts: '.'

--------------------------------------------------------------------------------

state 48 contains 1 shift/reduce conflicts.

	  *** r_value0 -> 'self' .                                (rule 49)
	      l_value -> 'self' . '.' l_value0                    (rule 51)

    Conflicts: '.'

--------------------------------------------------------------------------------

state 121 contains 1 shift/reduce conflicts.

	  *** l_value -> 'self' '.' l_value0 .
	      l_value0 -> l_value0 . '.' ID_VAR

    Conflicts: '.'

--------------------------------------------------------------------------------

state 168 contains 1 shift/reduce conflicts.

	      if_stmt -> 'if' statement 'then' statement . maybe_else_stmt

    Conflicts: 'else'

Example:
    if a then if b; then c; else d;

Shift parses as:
    if a then { if b then c; else d; }

--------------------------------------------------------------------------------
-}

%token
  'data'        { Token _ TokData }
  'impl'        { Token _ TokImpl }
  'def'         { Token _ TokDef }
  'val'         { Token _ TokVal }
  'var'         { Token _ TokVar }
  'self'        { Token _ TokSelf }
  'if'          { Token _ TokIf }
  'then'        { Token _ TokThen }
  'else'        { Token _ TokElse }
  'loop'        { Token _ TokLoop }
  'break'       { Token _ TokBreak }
  'continue'    { Token _ TokContinue }
  ';'           { Token _ TokSemi }
  '.'           { Token _ TokDot }
  '!'           { Token _ TokBang }
  '='           { Token _ TokAssign }
  '{'           { Token _ TokBraceOpen }
  '}'           { Token _ TokBraceClose }
  '('           { Token _ TokParenOpen }
  ')'           { Token _ TokParenClose }
  ','           { Token _ TokComma }
  ':'           { Token _ TokColon }
  '::'          { Token _ TokDoubleColon }
  '+'           { Token _ (TokArithOp AoAdd) }
  '-'           { Token _ (TokArithOp AoSub) }
  '*'           { Token _ (TokArithOp AoMul) }
  '/'           { Token _ (TokArithOp AoDiv) }
  '%'           { Token _ (TokArithOp AoRem) }
  ARITH_ASSIGN  { Token _ (TokArithAssign $$) }
  CMP_OP        { Token _ (TokCmpOp       $$) }
  LIT_INT       { Token _ (TokLitInt      $$) }
  LIT_DOUBLE    { Token _ (TokLitDouble   $$) }
  LIT_STRING    { Token _ (TokLitString   $$) }
  ID_DATA       { Token _ (TokIdData      $$) }
  ID_VAR        { Token _ (TokIdVar       $$) }
  LABEL         { Token _ (TokLabel       $$) }

%monad { P }
%lexer { (lexToken >>=) } { Token _ TokEof }
%tokentype { Token }
%error { parseFail }

%name parseFileP file

%%

file :: { File }
        : top_level_bindings                                    { File $1 }

top_level_bindings :: { Seq TopLevelBinding }
        : top_level_bindings top_level_binding                  { $1 `snoc` $2 }
        | {- empty -}                                           { empty }

top_level_binding :: { TopLevelBinding }
        : data                                                  { TlData   $1 }
        | impl                                                  { TlImpl   $1 }
        | function                                              { TlFunc   $1 }
        | va_binding                                            { TlVaBind $1 }

data :: { Data }
        : 'data' ID_DATA '=' '{' data_fields '}'                { Data $2 $5 }

data_fields :: { Seq VaDecl }
        : data_fields0                                          { $1 }
        | {- empty -}                                           { empty }

data_fields0 :: { Seq VaDecl }
        : data_fields0 va_decl                                  { $1 `snoc` $2 }
        | va_decl                                               { singleton $1 }

va_decl :: { VaDecl }
        : val_or_var ID_VAR type_sig ','                        { VaDecl $1 $2 $3 }

impl :: { Impl }
        : 'impl' ID_DATA '=' '{' impl_funcs '}'                 { Impl $2 $5 }

impl_funcs :: { Seq Function }
        : impl_funcs function                                   { $1 `snoc` $2 }
        | function                                              { singleton $1 }

function :: { Function }
        : 'def' ID_VAR '(' fn_params ')' type_sig '=' fn_body   { Function $2 $4 $6 $8 }

fn_params :: { FnParams }
        : 'self' ',' fnp_rest                                   { FnParams (Just Self) $3 }
        | 'self'                                                { FnParams (Just Self) empty }
        | fnp_rest                                              { FnParams Nothing $1 }
        | {- empty -}                                           { FnParams Nothing empty }

fnp_rest :: { Seq VaDecl }
        : fnp_rest ',' func_param                               { $1 `snoc` $3 }
        | func_param                                            { singleton $1 }

func_param :: { VaDecl }
        : implicit_val_or_var ID_VAR type_sig                   { VaDecl $1 $2 $3 }

implicit_val_or_var :: { ValOrVar }
        : val_or_var                                            { $1 }
        | {- empty -}                                           { Val }

fn_body :: { Either RValue StmtBlock }
        : r_value                                               { Left  $1 }
        | stmt_block                                            { Right $1 }

r_value :: { RValue }
        : constr                                                { RvConstr   $1 }
        | if_expr                                               { RvIf       $1 }
        | expr_block                                            { RvBlock    $1 }
        | r_value4                                              { $1 }

r_value4 :: { RValue }
        : r_value4 CMP_OP r_value3                              { RvBinOp (BoCo $2) $1 $3 }
        | r_value3                                              { $1 }

r_value3 :: { RValue }
        : r_value3 '+' r_value2                                 { RvBinOp (BoAo AoAdd) $1 $3 }
        | r_value3 '-' r_value2                                 { RvBinOp (BoAo AoSub) $1 $3 }
        | r_value2                                              { $1 }

r_value2 :: { RValue }
        : r_value2 '*' r_value1                                 { RvBinOp (BoAo AoMul) $1 $3 }
        | r_value2 '/' r_value1                                 { RvBinOp (BoAo AoDiv) $1 $3 }
        | r_value2 '%' r_value1                                 { RvBinOp (BoAo AoRem) $1 $3 }
        | r_value1                                              { $1 }

r_value1 :: { RValue }
        : '-' r_value0                                          { RvUnOp UoNeg $2 }
        | '!' r_value0                                          { RvUnOp UoNot $2 }
        | r_value0                                              { $1 }

r_value0 :: { RValue }
        : '(' r_value ')'                                       { $2 }
        | r_value0 '.' ID_VAR                                   { RvFieldAcc $1 $3 }
        | call                                                  { RvCall $1 }
        | l_value                                               { RvLVal $1 }
        | 'self'                                                { RvSelf Self }
        | literal                                               { RvLit $1 }

l_value :: { LValue }
        : 'self' '.' ID_VAR                                     { LvFieldAcc (Left Self) $3 }
        | l_value '.' ID_VAR                                    { LvFieldAcc (Right $1) $3 }
        | ID_VAR                                                { LvVa $1 }

expr_block :: { ExprBlock }
        : '{' statements r_value '}'                            { ExprBlock $2 $3 }
        | '{' r_value '}'                                       { ExprBlock empty $2 }

statements :: { Seq Statement }
        : statements statement                                  { $1 `snoc` $2 }
        | statement                                             { singleton $1 }

statement :: { Statement }
        : va_binding                                            { StVaBind $1 }
        | l_value '=' r_value ';'                               { StAssign $1 $3 }
        | l_value ARITH_ASSIGN r_value ';'                      { StArithAssign $1 $2 $3 }
        | maybe_labeled 'loop' statement                        { StLoop $1 $3 }
        | 'break' maybe_label ';'                               { StBreak $2 }
        | 'continue' maybe_label ';'                            { StContinue $2 }
        | if_stmt                                               { StIf $1 }
        | stmt_block                                            { StBlock $1 }
        | r_value ';'                                           { StExpr $1 }

va_binding :: { VaBinding }
        : val_or_var ID_VAR maybe_type_sig '=' r_value ';'      { VaBinding $1 $2 $3 $5 }

maybe_type_sig :: { Maybe IdData }
        : type_sig                                              { Just $1 }
        | {- empty -}                                           { Nothing }

maybe_labeled :: { Maybe Label }
        : LABEL ':'                                             { Just $1 }
        | {- empty -}                                           { Nothing }

maybe_label :: { Maybe Label }
        : LABEL                                                 { Just $1 }
        | {- empty -}                                           { Nothing }

stmt_block :: { StmtBlock }
        : '{' statements '}'                                    { StmtBlock $2 }
        | '{' '}'                                               { StmtBlock empty }

call :: { Call }
        : ID_DATA '::' ID_VAR '(' call_params ')'               { CallExplicit $1 $3 $5 }
        | r_value0 '(' call_params ')'                          { CallNormal $1 $3 }

call_params :: { Seq RValue }
        : call_params0                                          { $1 }
        | {- empty -}                                           { empty }

call_params0 :: { Seq RValue }
        : call_params0 ',' r_value                              { $1 `snoc` $3 }
        | r_value                                               { singleton $1 }

constr :: { Constr }
        : ID_DATA '{' ctr_fields '}'                            { Constr $1 $3 }
        | ID_DATA                                               { Constr $1 empty }

ctr_fields :: { Seq (IdVar,RValue) }
        : ctr_fields0                                           { $1 }
        | {- empty -}                                           { empty }

ctr_fields0 :: { Seq (IdVar,RValue) }
        : ctr_fields0 ctr_field                                 { $1 `snoc` $2 }
        | ctr_field                                             { singleton $1 }

ctr_field :: { (IdVar,RValue) }
        : ID_VAR '=' r_value ','                                { ($1,$3) }

if_expr :: { IfExpr }
        : 'if' r_value 'then' r_value 'else' r_value            { IfExpr $2 $4 $6 }

if_stmt :: { IfStmt }
        : 'if' r_value 'then' statement maybe_else_stmt         { IfStmt $2 $4 $5 }

maybe_else_stmt :: { Maybe Statement }
        : 'else' statement                                      { Just $2 }
        | {- empty -}                                           { Nothing }

literal :: { Literal }
        : LIT_INT                                               { LitInt $1 }
        | LIT_DOUBLE                                            { LitDouble $1 }
        | LIT_STRING                                            { LitString $1 }
        | '(' ')'                                               { LitUnit }

val_or_var :: { ValOrVar }
        : 'val'                                                 { Val }
        | 'var'                                                 { Var }

type_sig :: { IdData }
        : ':' ID_DATA                                           { $2 }