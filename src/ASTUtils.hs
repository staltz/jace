module ASTUtils (
  isFact,
  isRule,
  isEvaluation,
  isVariable,
  groupByRelation,
  inferRelationArity,
  ) where

import Parser (AST(..), Term(..), Relation(..))
import qualified Data.Map as Map

(|>) = flip ($)

isFact :: AST -> Bool
isFact (Fact _) = True
isFact _ = False

isRule :: AST -> Bool
isRule (Rule _ _) = True
isRule _ = False

isEvaluation :: AST -> Bool
isEvaluation (Evaluation _) = True
isEvaluation _ = False

isVariable :: Term -> Bool
isVariable (Variable _) = True
isVariable _ = False

groupByRelation :: [AST] -> [(String, [AST])]
groupByRelation astList =
  astList
  |> keyify
  |> sortAndGroup
  |> Map.toList

  where
    keyify :: [AST] -> [(String, AST)]
    keyify (ast : rest) =
      case ast of
        Fact (Relation name _) -> (name, ast) : (keyify rest)
        Rule (Relation name _) _ -> (name, ast) : (keyify rest)
        Evaluation (Relation name _) -> (name, ast) : (keyify rest)
    keyify [] = []

    sortAndGroup :: [(String, AST)] -> Map.Map String [AST]
    sortAndGroup assocs = Map.fromListWith (++) [(k, [v]) | (k, v) <- assocs]

inferRelationArity :: AST -> Int
inferRelationArity ast =
  case ast of
    Fact (Relation _ terms) -> length terms
    Rule (Relation _ terms) _ -> length terms
    Evaluation (Relation _ terms) -> length terms
