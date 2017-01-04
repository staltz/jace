module Lexer (Token(..), tokenize) where

import qualified Text.Parsec as Parsec
import Data.List (intersperse)

(|>) = flip ($)

data Token =
  ParenOpenToken |
  ParenCloseToken |
  NameToken String |
  VarToken String |
  IntToken Int |
  EvalToken |
  IfToken |
  AndToken |
  CommaToken |
  PeriodToken
  deriving (Show, Eq)

tokenize :: String -> Either Parsec.ParseError [Token]
tokenize sourceCode =
  Parsec.parse slFileParser "" sourceCode
  |> flattenTokenList

slFileParser :: Parsec.Parsec String () [[Token]]
slFileParser = Parsec.manyTill statementParser Parsec.eof

statementParser = do
  Parsec.choice [
    Parsec.try clauseParser,
    Parsec.try evaluationParser,
    Parsec.try ruleParser
    ]

clauseParser = do
  tokens <- relationParser
  periodParser
  return (tokens ++ [PeriodToken])

evaluationParser = do
  Parsec.string "evaluate"
  Parsec.many1 Parsec.space
  relationTokens <- relationParser
  periodParser
  return ([EvalToken] ++ relationTokens ++ [PeriodToken])

ruleParser = do
  headRelationTokens <- relationParser
  Parsec.many1 Parsec.space
  Parsec.string "if"
  Parsec.many1 Parsec.space
  bodyTokens <- Parsec.sepBy relationParser andParser
  let bodyTokensWithAnds = (intersperse [AndToken] bodyTokens) >>= id
  periodParser
  return (headRelationTokens ++ [IfToken] ++ bodyTokensWithAnds ++ [PeriodToken])

relationParser = do
  relationName <- nameParser
  relationBody <- relationArgsParser
  return (relationName : relationBody)

relationArgsParser = do
  Parsec.char '('
  argTokens <- Parsec.sepBy literalOrVarParser commaParser
  Parsec.char ')'
  let argTokensWithCommas = intersperse CommaToken argTokens
  return ([ParenOpenToken] ++ argTokensWithCommas ++ [ParenCloseToken])

literalOrVarParser = do
  Parsec.choice [nameParser, varParser, intParser]

nameParser = do
  firstChar <- Parsec.lower
  rest <- Parsec.many Parsec.alphaNum
  let name = firstChar : rest
  return (NameToken name)

intParser = do
  digits <- Parsec.many1 Parsec.digit
  let int = read digits :: Int
  return (IntToken int)

varParser = do
  firstChar <- Parsec.upper
  rest <- Parsec.many Parsec.alphaNum
  let var = firstChar : rest
  return (VarToken var)

andParser = do
  Parsec.spaces
  Parsec.string "and"
  Parsec.spaces
  return AndToken

commaParser = do
  Parsec.spaces
  Parsec.char ','
  Parsec.spaces
  return CommaToken

periodParser = do
  Parsec.spaces
  Parsec.char '.'
  Parsec.spaces
  return PeriodToken

flattenTokenList :: Either Parsec.ParseError [[Token]] -> Either Parsec.ParseError [Token]
flattenTokenList (Left err) = Left err
flattenTokenList (Right tokens) = Right (tokens >>= id)
