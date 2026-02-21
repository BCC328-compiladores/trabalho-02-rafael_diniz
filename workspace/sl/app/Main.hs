module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Tree (Tree(..), drawTree)
import Frontend.Lexer.SlLexer (lexer)
import Frontend.Parser.SlParser (slParser)
import Frontend.Syntax.SlSyntax

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--lexer", filename] -> runLexer filename
        ["--parser", filename] -> runParser filename
        _ -> do
            putStrLn "Usage:"
            putStrLn "  program --lexer <filename>   : Run lexer and print tokens"
            putStrLn "  program --parser <filename>  : Run parser and print AST"
            exitFailure

runLexer :: FilePath -> IO ()
runLexer filename = do
    content <- readFile filename
    case lexer content of
        Left err -> do
            putStrLn $ "Lexer error: " ++ err
            exitFailure
        Right tokens -> mapM_ print tokens

runParser :: FilePath -> IO ()
runParser filename = do
    content <- readFile filename
    result <- slParser content
    case result of
        Left err -> do
            putStrLn $ "Parser error: " ++ err
            exitFailure
        Right ast -> putStrLn $ drawTree (astToTree ast)

-- Convert AST to Tree for pretty printing
astToTree :: Sl -> Tree String
astToTree (Sl stmts) = Node "Program" (map stmtToTree stmts)

stmtToTree :: Stmt -> Tree String
stmtToTree stmt = case stmt of
    SAssign var exp -> Node ("Assignment: " ++ var) [expToTree exp]
    SFunc mType args block -> Node ("Function" ++ maybe "" (" -> " ++) mType) 
                                   (argsToTree args : map stmtToTree block)
    SStruct name fields -> Node ("Struct: " ++ name) (map structFieldToTree fields)
    SIf exp thenBlock mElseBlock -> Node "If" $
        [expToTree exp, Node "Then" (map stmtToTree thenBlock)] ++
        maybe [] (\elseBlock -> [Node "Else" (map stmtToTree elseBlock)]) mElseBlock
    SWhile exp block -> Node "While" [expToTree exp, Node "Body" (map stmtToTree block)]
    SReturn mExp -> Node "Return" (maybe [] (\e -> [expToTree e]) mExp)
    SPrint exp -> Node "Print" [expToTree exp]
    SExp exp -> Node "Expression" [expToTree exp]

expToTree :: Exp -> Tree String
expToTree exp = case exp of
    EValue val -> Node ("Value: " ++ show val) []
    EVar var -> Node ("Variable: " ++ var) []
    ENot e -> Node "Not" [expToTree e]
    e1 :<: e2 -> Node "<" [expToTree e1, expToTree e2]
    e1 :>: e2 -> Node ">" [expToTree e1, expToTree e2]
    e1 :<=: e2 -> Node "<=" [expToTree e1, expToTree e2]
    e1 :>=: e2 -> Node ">=" [expToTree e1, expToTree e2]
    e1 :=: e2 -> Node "==" [expToTree e1, expToTree e2]
    e1 :/=: e2 -> Node "!=" [expToTree e1, expToTree e2]
    e1 :|: e2 -> Node "||" [expToTree e1, expToTree e2]
    e1 :&: e2 -> Node "&&" [expToTree e1, expToTree e2]
    e1 :+: e2 -> Node "+" [expToTree e1, expToTree e2]
    e1 :-: e2 -> Node "-" [expToTree e1, expToTree e2]
    e1 :*: e2 -> Node "*" [expToTree e1, expToTree e2]
    e1 :/: e2 -> Node "/" [expToTree e1, expToTree e2]
    e1 :%: e2 -> Node "%" [expToTree e1, expToTree e2]
    ECall func args -> Node ("Call: " ++ func) (map expToTree args)
    EArrayAccess arr idx -> Node "Array Access" [expToTree arr, expToTree idx]
    EFieldAccess struct field -> Node ("Field Access: ." ++ field) [expToTree struct]
    EStructConstruct name values -> Node ("Struct Constructor: " ++ name) (map expToTree values)

argsToTree :: [Arg] -> Tree String
argsToTree args = Node "Arguments" (map argToTree args)
  where
    argToTree (var, typ) = Node (var ++ " : " ++ typ) []

structFieldToTree :: (FieldName, Type) -> Tree String
structFieldToTree (field, typ) = Node (field ++ " : " ++ typ) []