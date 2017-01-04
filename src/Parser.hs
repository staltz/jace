module Parser (
  AST(..),
  Term(..),
  Relation(..),
  parse) where

import Lexer (tokenize, Token(..))

(|>) = flip ($)

data AST =
  Evaluation Relation |
  Fact Relation |
  Rule Relation [Relation]
  deriving (Show, Eq)

-- Relation includes a name and a list of literals or variables
data Relation =
  Relation String [Term]
  deriving (Show, Eq)

data Term = Variable String | Atom String | IntLiteral Int
  deriving (Show, Eq)

parse :: [Token] -> Maybe [AST]
parse tokens =
  case tokens of
    (EvalToken : _) -> parseEvaluation tokens
    (NameToken _ : _) -> parseFactOrRule tokens
    [] -> Just []
    _ -> Nothing

parseEvaluation :: [Token] -> Maybe [AST]
parseEvaluation (EvalToken : tokens) =
  case parseRelation tokens of
    Just (relation, (PeriodToken : rest)) ->
      parse rest
      |> fmap (\restAst -> (Evaluation relation : restAst))
    _ -> Nothing
parseEvaluation _ = Nothing

parseFactOrRule :: [Token] -> Maybe [AST]
parseFactOrRule tokens =
  let
    relationAndRest = parseRelation tokens
  in
    case relationAndRest of
      Just (_, (PeriodToken : _)) -> parseFact relationAndRest
      Just (_, (IfToken : _)) -> parseRule relationAndRest
      _ -> Nothing

parseFact :: Maybe (Relation, [Token]) -> Maybe [AST]
parseFact (Just (relation, (PeriodToken : rest))) =
  parse rest
  |> fmap (\restAst -> (Fact relation : restAst))
parseFact _ = Nothing

parseRule :: Maybe (Relation, [Token]) -> Maybe [AST]
parseRule (Just (headRelation, (IfToken : rest))) =
  parseRuleBody rest []
  >>= (\(relations, remaining) ->
    parse remaining
    |> fmap (\restAst -> (Rule headRelation relations) : restAst)
    )
parseRule _ = Nothing

parseRuleBody ::
  [Token] -> -- input of tokens
  [Relation] -> -- accumulated relations as partial solution
  Maybe ([Relation], [Token]) -- solution and remaining tokens
parseRuleBody tokens acc =
  case parseRelation tokens of
    Just (relation, (AndToken : rest)) -> parseRuleBody rest (acc ++ [relation])
    Just (relation, (PeriodToken : rest)) -> Just ((acc ++ [relation]), rest)
    _ -> Nothing

parseRelation ::
  [Token] -> -- original list of tokens, starting with a name
  Maybe (Relation, [Token]) -- solution and remaining tokens
parseRelation (NameToken name : rest) =
  parseRelationArgs rest []
  |> fmap (\(args, remaining) -> (Relation name args, remaining))
parseRelation _ = Nothing

parseRelationArgs ::
  [Token] -> -- original list of tokens, starting with open paren
  [Term] -> -- accumulated arguments as partial solution
  Maybe ([Term], [Token]) -- solution and remaining tokens
parseRelationArgs (token : rest) acc =
  case token of
    ParenOpenToken -> if length acc == 0 then parseRelationArgs rest [] else Nothing
    NameToken name -> parseRelationArgs rest (acc ++ [Atom name])
    VarToken var -> parseRelationArgs rest (acc ++ [Variable var])
    IntToken int -> parseRelationArgs rest (acc ++ [IntLiteral int])
    CommaToken -> parseRelationArgs rest acc
    ParenCloseToken -> Just (acc, rest)
    _ -> Nothing
parseRelationArgs _ _ = Nothing
