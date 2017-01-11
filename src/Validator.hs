module Validator (validate) where

import Parser (AST(..), Term(..), Relation(..))
import ASTUtils (isRule, isFact, groupByRelation, inferRelationArity, isVariable)
import Data.Maybe (isJust)
import qualified Data.List as List

(|>) = flip ($)

type ValidationError = String

validate :: [AST] -> Either ValidationError [AST]
validate astList =
  astList
  |> checkEach
  |> (=<<) checkOverall

-- Checks whether each statement looks ok, in isolation.
checkEach :: [AST] -> Either ValidationError [AST]
checkEach astList =
  case foldl check Nothing astList of
    Nothing -> Right astList
    Just err -> Left err

  where
    check :: Maybe ValidationError -> AST -> Maybe ValidationError
    check prev ast =
      case prev of
        Just err -> Just err
        Nothing ->
          case ast of
            Evaluation _ -> checkEvaluation ast
            Fact _ -> checkFact ast
            Rule _ _ -> checkRule ast

-- Checks whether an evaluation looks ok, in isolation.
-- An evaluation should have at least one variable in the terms.
checkEvaluation :: AST -> Maybe ValidationError
checkEvaluation ast =
  case ast of
    Evaluation (Relation relationName terms) ->
      if (any isVariable terms) then
        Nothing
      else
        Just ("An evaluation should have at least one variable, check this out: " ++ show ast)
    _ -> Nothing

-- Checks whether a fact looks ok, in isolation.
-- Facts should have only constants, no variables.
checkFact :: AST -> Maybe ValidationError
checkFact ast =
  case ast of
    Fact (Relation relationName terms) ->
      if (all termIsConcrete terms) then
        Nothing
      else
        Just ("Fact should NOT have a variable, check this out: " ++ show ast)
    _ ->
      Just "This is weird, the validator got confused"

  where
    termIsConcrete :: Term -> Bool
    termIsConcrete term =
      case term of
        Atom _ -> True
        IntLiteral _ -> True
        Variable _ -> False

-- Checks whether a rule looks ok, in isolation.
checkRule :: AST -> Maybe ValidationError
checkRule ast =
  case checkRuleOnlyHasVars ast of
    Just err -> Just err
    Nothing ->
      checkAllRuleHeadVarsInBody ast

-- Checks whether the relations in a rule only contain variables.
checkRuleOnlyHasVars :: AST -> Maybe ValidationError
checkRuleOnlyHasVars ast =
  case ast of
    Rule relHead body ->
      if (relationOnlyHasVars relHead && all relationOnlyHasVars body) then
        Nothing
      else
        Just ("Rule should only have variables, check this out: " ++ show ast)

  where
    relationOnlyHasVars :: Relation -> Bool
    relationOnlyHasVars (Relation _ terms) =
      all isVariable terms

-- Checks whether all head variables of a rule are in the body relations too.
checkAllRuleHeadVarsInBody :: AST -> Maybe ValidationError
checkAllRuleHeadVarsInBody ast =
  case ast of
    Rule (Relation headName headTerms) bodyRelations ->
      let
        passed =
          headTerms
          |> filter isVariable
          |> all (isVariableIn bodyRelations)
      in
        if passed then
          Nothing
        else
          Just ("Rule should include all head variables in the body, check this out: " ++ show ast)
    _ ->
      Just "This is weird, the validator got confused"

  where
    isVariableIn :: [Relation] -> Term -> Bool
    isVariableIn relations variable =
      any (\(Relation _ terms) -> elem variable terms) relations

-- Checks whether the program looks ok cross-statements.
checkOverall :: [AST] -> Either ValidationError [AST]
checkOverall astList =
  astList
  |> checkEachRelationArity
  |> (=<<) checkEachRuleHasNoFacts
  |> (=<<) checkRelatedRulesHaveSameHead

-- Checks whether all statements of a relation have the same arity.
checkEachRelationArity :: [AST] -> Either ValidationError [AST]
checkEachRelationArity astList =
  let
    errors = detectEachArityError astList
    errorReport = reportErrors errors
  in
    if any isJust errors then
      Left errorReport
    else
      Right astList

  where
    detectEachArityError :: [AST] -> [Maybe ValidationError]
    detectEachArityError astList =
      astList
      |> groupByRelation
      |> map detectArityError

    detectArityError :: (String, [AST]) -> Maybe ValidationError
    detectArityError relation =
      if isRelationArityConsistent relation then
        Nothing
      else
        Just ("Inconsistent arity for relation " ++ fst relation)

    isRelationArityConsistent :: (String, [AST]) -> Bool
    isRelationArityConsistent (_, astList) =
      astList
      |> map inferRelationArity
      |> foldl compareArity (-1, True)
      |> snd

    compareArity :: (Int, Bool) -> Int -> (Int, Bool)
    compareArity (_, False) x1 = (x1, False)
    compareArity (x0, True) x1 =
      let
        isEq = x0 == x1
      in
        if x0 == -1 then
          (x1, True)
        else
          (x1, isEq)

    reportErrors :: [Maybe ValidationError] -> ValidationError
    reportErrors errors =
      errors
      |> filter isJust
      |> map (\x -> case x of Just err -> err; Nothing -> "")
      |> foldl (++) ""

-- Checks whether each rule is the only statement for that relation.
checkEachRuleHasNoFacts :: [AST] -> Either ValidationError [AST]
checkEachRuleHasNoFacts astList =
  let
    passed =
      astList
      |> filter isRule
      |> all (ruleHasNoFacts astList)
  in
    if passed then
      Right astList
    else
      Left "If there is a rule for a relation, there cannot be facts for that relation"

  where
    ruleHasNoFacts :: [AST] -> AST -> Bool
    ruleHasNoFacts astList (Rule (Relation relationName _) _) =
      astList
      |> filter isFact
      |> all (\fact -> not (isFactName relationName fact))
    ruleHasNoFacts _ _ = False

    isFactName :: String -> AST -> Bool
    isFactName name (Fact (Relation factName _)) = name == factName
    isFactName _ _ = False

-- Checks whether all rules of the same relation have the same head,
-- with the same variable names and order.
checkRelatedRulesHaveSameHead :: [AST] -> Either ValidationError [AST]
checkRelatedRulesHaveSameHead astList =
  let
    passed =
      astList
      |> filter isRule
      |> groupByRelation
      |> all haveSameHead
  in
    if passed then
      Right astList
    else
      Left "All rules for the same relation should have the same type of head"

  where
    haveSameHead :: (String, [AST]) -> Bool
    haveSameHead (_, rules) =
      rules
      |> map statementHead
      |> List.nub
      |> (\list -> length list == 1)

    statementHead :: AST -> Relation
    statementHead (Rule ruleHead _) = ruleHead
    statementHead (Fact rel) = rel
    statementHead (Evaluation rel) = rel