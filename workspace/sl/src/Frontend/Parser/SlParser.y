{
module Frontend.Parser.SlParser (slParser, parserTest) where


import Utils.Value

import Frontend.Lexer.Token
import Frontend.Lexer.SlLexer hiding (lexer)
import Frontend.Syntax.SlSyntax
}


%name parser Sl
%monad         {Alex}{(>>=)}{return}
%tokentype     { Token }
%error         { parseError }
%lexer         {lexer}{Token _ TEOF}

%token
    'let'      {Token _ TLet}
    ':'        {Token _ TAssign}
    '='        {Token _ TAttribution}
    var        {Token _ (TIdent $$)}
    '('        {Token _ TLParen}
    ')'        {Token _ TRParen}
    ';'        {Token _ TSemi}
    'func'     {Token _ TFunc }
    ','        {Token _ TComma}
    'struct'   {Token _ TStruct}
    'if'       {Token _ TIf}
    'else'     {Token _ TElse}
    'while'    {Token _ TWhile}
    'return'   {Token _ TReturn}
    '{'        {Token _ TLBrace}
    '}'        {Token _ TRBrace}
    '['        {Token _ TLBracket}
    ']'        {Token _ TRBracket}
    '.'        {Token _ TDot}
    'print'    {Token _ TPrint}
    'int'      {Token _ TInt}
    intLit     {Token _ (LitInt $$)}
    'float'    {Token _ TFloat}
    floatLit   {Token _ (LitFloat $$)}
    'void'     {Token _ TVoid}
    'bool'     {Token _ TBool}
    'true'     {Token _ TTrue}
    'false'    {Token _ TFalse}
    'string'   {Token _ TString}
    stringLit  {Token _ (LitString $$)}
    '+'        {Token _ TPlus}
    '-'        {Token _ TMinus}
    '*'        {Token _ TTimes}
    '/'        {Token _ TDiv}
    '%'        {Token _ TMod}
    '!'        {Token _ TNot}
    '=='       {Token _ TEq}
    '!='       {Token _ TNeq}
    '<'        {Token _ TLt}
    '>'        {Token _ TGt}
    '<='       {Token _ TLeq}
    '>='       {Token _ TGeq}
    '&&'       {Token _ TAnd}
    '||'       {Token _ TOr}
--    'read'     {Token _ TRead}

-- Operator precedence and associativity (from lowest to highest)
%right         '='
%left          '||'
%left          '&&'
%nonassoc      '==' '!='
%nonassoc      '<' '>' '<=' '>='
%left          '+' '-'
%left          '*' '/' '%'
%right         '!'
%left          '.' '[' ']' '(' ')'

%%

-- Main program structure
Sl :  Stmts              { Sl $1 }

-- Statement list
Stmts : Stmt Stmts       { $1 : $2 }
      | {- empty -}      { [] }

-- Individual statements
Stmt : 'let' var ':' Type '=' Exp ';'                       { SAssign $2 $6 }
     | 'let' var ':' Type ';'                               { SDecl $2 $4 }
     | 'func' var '(' Params ')' '{' Stmts '}'              { SFunc Nothing $4 $7 }
     | 'func' var '(' Params ')' ':' Type '{' Stmts '}'     { SFunc (Just $7) $4 $9 }
     | 'struct' var '{' StructFields '}'                    { SStruct $2 $4 }
     | 'if' '(' Exp ')' '{' Stmts '}' 'else' '{' Stmts '}'  { SIf $3 $6 (Just $10) }
     | 'if' '(' Exp ')' '{' Stmts '}'                       { SIf $3 $6 Nothing }
     | 'while' '(' Exp ')' '{' Stmts '}'                    { SWhile $3 $6 }
     | 'return' Exp ';'                                     { SReturn (Just $2) }
     | 'return' ';'                                         { SReturn Nothing }
     | 'print' '(' Exp ')' ';'                              { SPrint $3 }
--     | 'read' '(' var ')' ';'                               { ERead $3 }
     | Exp ';'                                              { SExp $1 }

-- Function Parameters and Arguments
Params : Param ',' Params     { $1 : $3 }
       | Param                { [$1] }
       | {- empty -}          { [] }

Param : var ':' Type          { ($1, $3) }
      | var                   { ($1, "int") }


-- Struct field definitions
StructFields : StructField ';' StructFields    { $1 : $3 }
             | {- empty -}                     { [] }

StructField : var ':' Type     { ($1, $3) }

Args :  Exp ',' Args          { $1 : $3 }
     | Exp                    { [$1] }
     | {- empty -}            { [] }

-- Types
Type : 'int'     { "int" }
     | 'float'   { "float" }
     | 'bool'    { "bool" }
     | 'string'  { "string" }
     | 'void'    { "void" }
     | var       { $1 }           -- custom types (structs)
     | var '[' intLit ']'  { $1 ++ "[" ++ show $3 ++ "]" }  -- array types

-- Expressions (properly structured to eliminate conflicts)
Exp : BinaryExp                    { $1 }

BinaryExp : 
       UnaryExp                    { $1 }
     | BinaryExp '=' BinaryExp     { EAssign $1 $3 }
     | BinaryExp '==' BinaryExp    { $1 :=: $3 }
     | BinaryExp '!=' BinaryExp    { $1 :/=: $3 }
     | BinaryExp '<' BinaryExp     { $1 :<: $3 }
     | BinaryExp '>' BinaryExp     { $1 :>: $3 }
     | BinaryExp '<=' BinaryExp    { $1 :<=: $3 }
     | BinaryExp '>=' BinaryExp    { $1 :>=: $3 }
     | BinaryExp '&&' BinaryExp    { $1 :&: $3 }
     | BinaryExp '||' BinaryExp    { $1 :|: $3 }
     | BinaryExp '+' BinaryExp     { $1 :+: $3 }
     | BinaryExp '-' BinaryExp     { $1 :-: $3 }
     | BinaryExp '*' BinaryExp     { $1 :*: $3 }
     | BinaryExp '/' BinaryExp     { $1 :/: $3 }
     | BinaryExp '%' BinaryExp     { $1 :%: $3 }

UnaryExp :
       PrimaryExp                  { $1 }
     | '!' UnaryExp                { ENot $2 }

-- Primary expressions and postfix operations
PrimaryExp : 
       AtomExp                     { $1 }
     | PrimaryExp '[' Exp ']'      { EArrayAccess $1 $3 }     -- array access  
     | PrimaryExp '.' var          { EFieldAccess $1 $3 }     -- field access

AtomExp :
       intLit                      { EValue (VInt $1) }
     | floatLit                    { EValue (VInt (round $1)) }  -- temporary conversion
     | stringLit                   { EValue (VString $1) }
     | 'true'                      { EValue (VBool True) }
     | 'false'                     { EValue (VBool False) }
     | var                         { EVar $1 }
     | var '(' Args ')'            { ECall $1 $3 }
     | var '{' Args '}'            { EStructConstruct $1 $3 } -- struct construction
     | '(' Exp ')'                 { $2 }

{
-- Test function for debugging
parserTest :: String -> IO ()
parserTest s = do
  r <- slParser s
  print r

-- Error handling
parseError :: Token -> Alex a
parseError (Token (line, col) lexeme) = 
  alexError $ "Parse error while processing lexeme: " ++ show lexeme
              ++ "\n at line " ++ show line ++ ", column " ++ show col

-- Lexer wrapper
lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)

-- Export the main parser function
slParser :: String -> IO (Either String Sl)
slParser content = do
  pure $ runAlex content parser
}