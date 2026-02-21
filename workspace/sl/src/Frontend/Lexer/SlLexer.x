{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Frontend.Lexer.SlLexer where

import Control.Monad
import Data.Char (chr, ord)
import Data.Maybe (fromJust)
import Numeric (readDec)
import Frontend.Lexer.Token
}

%wrapper "monadUserState"

$digit = 0-9            -- digits
$letter =[a-zA-Z]      -- letters

-- second RE macros
@float   = $digit+ "." $digit+
@int     = $digit+
@string  = \" [$letter $digit\ \t\b]* \"
@identifier = $letter[$letter $digit]*

-- tokens declarations  
tokens :-
      -- whitespace and line comments
      <0> $white+       ;
      <0> "//" .*       ;

      -- other tokens
      <0> "="         { simpleToken TAttribution }
      <0> "("         { simpleToken TLParen }
      <0> ")"         { simpleToken TRParen }
      <0> "+"         { simpleToken TPlus }
      <0> "*"         { simpleToken TTimes }
      <0> "-"         { simpleToken TMinus }
      <0> "/"         { simpleToken TDiv }
      <0> "%"         { simpleToken TMod }
      <0> "!"         { simpleToken TNot }
      <0> "=="        { simpleToken TEq }
      <0> "!="        { simpleToken TNeq }
      <0> "<"         { simpleToken TLt }
      <0> ">"         { simpleToken TGt }
      <0> ">="        { simpleToken TGeq }
      <0> "<="        { simpleToken TLeq }
      <0> "&&"        { simpleToken TAnd }
      <0> "||"        { simpleToken TOr }
      <0> ";"         { simpleToken TSemi }
      <0> ","         { simpleToken TComma }
      <0> "if"        { simpleToken TIf }
      <0> "else"      { simpleToken TElse }
      <0> "while"     { simpleToken TWhile }
      <0> "return"    { simpleToken TReturn }
      <0> "print"     { simpleToken TPrint }
      <0> "read"      { simpleToken TRead }
      <0> "let"       { simpleToken TLet}
      <0> "int"       { simpleToken TInt }
      <0> "float"     { simpleToken TFloat }
      <0> "bool"      { simpleToken TBool }
      <0> "string"    { simpleToken TString }
      <0> "void"      { simpleToken TVoid }
      <0> "struct"    { simpleToken TStruct }
      <0> "true"      { simpleToken TTrue }
      <0> "false"     { simpleToken TFalse }
      <0> "func"      { simpleToken TFunc }
      <0> "{"         { simpleToken TLBrace }
      <0> "}"         { simpleToken TRBrace }
      <0> "["         { simpleToken TLBracket }
      <0> "]"         { simpleToken TRBracket }
      <0> "."         { simpleToken TDot }

      -- string handling
      <0>             \"            { enterNewString `andBegin` state_string }
      <state_string>  \\n           { addCharToString '\n' }
      <state_string>  \\t           { addCharToString '\t' }

      <state_string>  \\\^[@-_]     { addControlToString }
      <state_string>  \\$digit$digit$digit
                                    { addAsciiToString }
      <state_string>  \\\"          { addCharToString '\"' }
      <state_string>  \\\\          { addCharToString '\\' }
      <state_string>  \\[\ \n\t\f\r\b\v]+\\
                                  ;
      <state_string>  \\            { \_ _ -> alexError "Illegal escape sequence" }
      <state_string>  \"            { leaveString `andBegin` state_initial }
      <state_string>  .             { addCurrentToString }
      <state_string>  \n            { skip }

      -- literals  
      <0> @float      { mkFloatLiteral }
      <0> @int        { mkIntLiteral }

      -- Variable declaration (identifiers must come after all keywords)
      <0> @identifier       { mkIdent }
      <0> ":"               { simpleToken TAssign }

      -- multi-line comment
      <0>       "/*"              { begin comment_state }
      <0>       "*/"              {\ _ _ -> alexError "Error! Unexpected close comment!" }
      <comment_state> "*/"        { endComment }
      <comment_state> .           ;
      <comment_state> \n          ;

{

-- states

state_initial :: Int
state_initial = 0


enterNewString, leaveString, addCurrentToString, addAsciiToString, addControlToString :: AlexAction Token

enterNewString (p, _, _, _) _ =
    do setLexerStringState (Just p)
       setLexerStringValue ""
       alexMonadScan

leaveString _ _ =
    do s <- getLexerStringValue
       mp <- getLexerStringState  -- position where string literal started
       setLexerStringState Nothing
       return (Token (position (fromJust mp)) (LitString (reverse s)))
         -- Andreas Abel, 2023-04-14, https://github.com/haskell/alex/issues/180 :
         -- We return Nothing as parsed input because we did not keep track
         -- of all the characters we processed for lexing the string literal.
         -- Future work:
         -- Extend the lexer state with info that lets us reconstruct the lexed input here.       

addCurrentToString i@(_, _, _, input) len = addCharToString c i len
  where
    c = if (len == 1)
           then head input
           else error "Invalid call to addCurrentToString''"

-- if we are given the special form '\nnn'
addAsciiToString i@(_, _, _, input) len = if (v < 256)
                                          then addCharToString c i len
                                          else alexError ("Invalid ascii value : " ++ input)
  where
    s = if (len == 4)
           then drop 1 input
           else error "Invalid call to 'addAsciiToString'"
    r = readDec s
    v = if (length r == 1)
           then fst (head r)
           else error "Invalid call to 'addAsciiToString'"
    c = chr v

-- if we are given the special form '\^A'
addControlToString i@(_, _, _, input) len = addCharToString c' i len
  where
    c = if (len == 1)
           then head input
           else error "Invalid call to 'addControlToString'"
    v = ord c
    c' = if (v >= 64)
            then chr (v - 64)
            else error "Invalid call to 'addControlToString'"

addCharToString :: Char -> AlexAction Token
addCharToString c _     _   =
    do addCharToLexerStringValue c
       alexMonadScan

-- minimal user state (not used but required by monadUserState wrapper)
data AlexUserState = AlexUserState
  { lexerStringState :: Maybe AlexPosn
  , lexerStringValue :: String
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
                   { lexerStringState   = Nothing
                   , lexerStringValue   = ""
                   }

getLexerStringState :: Alex (Maybe AlexPosn)
getLexerStringState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringState ust)

setLexerStringState :: Maybe AlexPosn -> Alex ()
setLexerStringState ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringState=ss}}, ())

getLexerStringValue :: Alex String
getLexerStringValue = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringValue ust)

setLexerStringValue :: String -> Alex ()
setLexerStringValue ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=ss}}, ())

addCharToLexerStringValue :: Char -> Alex ()
addCharToLexerStringValue c = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=c:lexerStringValue (alex_ust s)}}, ())

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f
  = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())

-- definition of the EOF token
alexEOF :: Alex Token
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  pure $ Token (position pos) TEOF

-- comment handling functions
endComment :: AlexAction Token
endComment input len
  = do
      alexSetStartCode 0
      skip input len

-- end variable declaration state
{-
tokenAndEnd :: AlexAction -> alexScan
tokenAndEnd input len = do
  alexSetStartCode 0  -- Exit to initial state
  simpleToken Token input len  -- Generate token
-}

position :: AlexPosn -> (Int, Int)
position (AlexPn _ x y) = (x,y)



-- make literal tokens
mkIntLiteral :: AlexAction Token
mkIntLiteral (st, _, _, str) len
  = pure $ Token (position st) (LitInt $ read $ take len str)

mkFloatLiteral :: AlexAction Token
mkFloatLiteral (st, _, _, str) len
  = pure $ Token (position st) (LitFloat $ read $ take len str)

mkIdent :: AlexAction Token
mkIdent (st, _, _, str) len
  = pure $ Token (position st) (TIdent (take len str))

simpleToken :: Lexeme -> AlexAction Token
simpleToken lx (st, _, _, _) _
  = return $ Token (position st) lx

-- lexer main function
lexer :: String -> Either String [Token]
lexer s = runAlex s go
  where
    go = do
      output <- alexMonadScan
      if lexeme output == TEOF then
        pure [output]
      else (output :) <$> go

}