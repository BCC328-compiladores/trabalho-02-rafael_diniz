module Frontend.Lexer.Token where

-- token definition

data Token
  = Token {
      pos :: (Int, Int)
    , lexeme :: Lexeme
    } deriving (Eq, Ord, Show)


-- lexeme definitions
data Lexeme
    = TLet | TAssign | TAttribution | TIdent { out :: String } 
    | TLParen | TRParen | TSemi | TComma | TFunc   | TStruct
    | TIf | TElse | TWhile | TReturn | TLBrace | TRBrace 
    | TLBracket | TRBracket  | TPrint | TRead | TDot
    -- type tokens
    | TInt | TFloat | TBool | TVoid | TString          
    -- literal tokens
    | LitInt Int | LitFloat Double | TTrue | TFalse
    | LitString { out :: String }
    -- operators tokens
    | TPlus | TMinus | TTimes | TDiv | TMod
    -- relational operators tokens
    | TNot | TEq | TNeq | TLt | TGt | TLeq | TGeq | TAnd | TOr
     -- end of file token
    | TEOF
    deriving (Eq, Ord, Show)