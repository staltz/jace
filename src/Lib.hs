module Lib (compile) where

import Lexer (Token, tokenize)
import Parser (AST, parse)
import Validator (validate)
import Transformer (transform)
import Writer (write)
import qualified Text.Parsec as Parsec
-- import Debug.Trace (trace)

-- Comment the below to enable debugging
trace :: String -> a -> a
trace _ x = x

(|>) = flip ($)

compile :: String -> String
compile s =
  tokenize s
  |> postTokenize

postTokenize :: Either Parsec.ParseError [Token] -> String
postTokenize (Left err) = "Error when compiling: " ++ show err
postTokenize (Right tokens) =
  trace ("Lexer: " ++ show tokens ++ "\n") (parse tokens)
  |> postParse

postParse :: Maybe [AST] -> String
postParse Nothing = "Error when parsing."
postParse (Just astList) =
  trace ("Parser: " ++ show astList ++ "\n") (validate astList)
  |> postValidate

postValidate :: Either String [AST] -> String
postValidate (Left err) = "Error when validating: " ++ show err
postValidate (Right astList) =
  trace "Validated. \n" (transform astList)
  |> postTransform

postTransform :: [AST] -> String
postTransform astList =
  trace ("Transformer: " ++ show astList ++ "\n") (write astList)
  |> postWrite

postWrite :: String -> String
postWrite jsCode = trace ("Writer: " ++ jsCode) jsCode
