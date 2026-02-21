module Frontend.Syntax.SlSyntax where

import Utils.Value

data Sl
  = Sl [Stmt]
    deriving (Eq, Ord, Show)

type Var = String
type Func = String
type Block = [Stmt]
type Type = String
type StructName = String
type FieldName = String

-- Arguments for functions
type Arg = (Var, Type)

-- Struct field definition
type StructField = (FieldName, Type)

data Stmt
  = SAssign Var Exp                   -- variable assignment: let var : type = exp;
  | SDecl Var Type                    -- variable declaration: let var : type;
--  | SArrayDecl Var Type Exp         -- array declaration: let var : type[exp];
  | SFunc (Maybe Type) [Arg] Block    -- function definition: func (args) : returnType { block }
  | SStruct StructName [StructField]  -- struct definition: struct Name { fields }
  | SIf Exp Block (Maybe Block)       -- if-else statement
  | SWhile Exp Block                  -- while loop
  | SReturn (Maybe Exp)               -- return statement
  | SPrint Exp                        -- print statement
  | SExp Exp                          -- expression statement
  deriving (Eq, Ord, Show)

data Exp
  = EValue Value 
  | EVar Var 
  | ENot Exp 
  | EAssign Exp Exp       -- assignment: lvalue = rvalue
  | Exp :<: Exp       -- less than
  | Exp :>: Exp       -- greater than  
  | Exp :<=: Exp      -- less than or equal
  | Exp :>=: Exp      -- greater than or equal
  | Exp :=: Exp       -- equality
  | Exp :/=: Exp      -- inequality
  | Exp :|: Exp       -- logical or
  | Exp :&: Exp       -- logical and
  | Exp :+: Exp       -- addition
  | Exp :-: Exp       -- subtraction
  | Exp :*: Exp       -- multiplication
  | Exp :/: Exp       -- division
  | Exp :%: Exp       -- modulo
  | ECall Func [Exp]  -- function call
  | EArrayAccess Exp Exp              -- array access: arr[index]
  | EFieldAccess Exp FieldName        -- field access: struct.field
  | EStructConstruct StructName [Exp] -- struct construction: Struct{values}
--  | ERead           -- read input
  deriving (Eq, Ord, Show)