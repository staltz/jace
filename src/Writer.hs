module Writer (write) where

import Parser (AST(..), Term(..), Relation(..))
import ASTUtils (isRule, isFact, isEvaluation, groupByRelation, inferRelationArity, isVariable)
import qualified Data.Map as Map
import Data.List (uncons, intersperse, nub, (\\))
import qualified Debug.Trace as Debug

(|>) = flip ($)

write :: [AST] -> String
write astList =
  let
    (facts, rules, evaluations) = classify astList
    factsBlock = writeAllFacts facts
    rulesBlock = writeAllRules rules
    varsBlock = writeEvaluationVariables evaluations
    evalsBlock = writeAllEvaluations evaluations
  in
    header ++
    "\n" ++
    factsBlock ++
    "\n" ++
    rulesBlock ++
    "\n" ++
    varsBlock ++
    evalsBlock

header :: String
header =
  "var $logic = require('logicjs');\n" ++
  "var $or = $logic.or, $and = $logic.and, $eq = $logic.eq, $lvar = $logic.lvar;\n" ++
  "function $report(t, vns, vss){" ++
    "console.log('evaluate '+t+':\\n'+" ++
      "vss.map(function(vs,j){" ++
        "return'    '+(j+1)+'. '+vs.map(function(v,i){" ++
          "return vns[i]+'='+v;" ++
        "}).join(', ');" ++
      "}).join('\\n')" ++
    ");" ++
  "}\n"

classify :: [AST] -> ([AST], [AST], [AST])
classify astList =
  let
    facts = filter isFact astList
    rules = filter isRule astList
    evaluations = filter isEvaluation astList
  in
    (facts, rules, evaluations)

writeAllFacts :: [AST] -> String
writeAllFacts facts =
  facts
  |> reverse
  |> groupByRelation
  |> map writeFactGroup
  |> intersperse "\n"
  |> join

  where
    writeFactGroup :: (String, [AST]) -> String
    writeFactGroup (name, facts) =
      let
        factsWritten = facts |> map writeFact
      in
        "function " ++ name ++ "(" ++ (makeFactGroupArguments facts) ++ ") {\n" ++
        (if length facts == 1 then
          "  return " ++ (factsWritten |> join) ++ ";\n"
        else
          "  return $or(\n" ++
          "    " ++ (factsWritten |> intersperse ",\n    " |> join) ++ "\n" ++
          "  );\n"
        ) ++
        "}\n"

    writeFact :: AST -> String
    writeFact ast =
      case ast of
        Fact (Relation _ terms) ->
          case uncons terms of
            Just (term, []) ->
              writeFactTermEquation("X1", term)
            Just (_, _) ->
              "$and(" ++
                (ast
                |> inferRelationArity
                |> makeVariables
                |> zip terms
                |> map (\(x,y) -> (y,x))
                |> map writeFactTermEquation
                |> intersperse ", "
                |> join) ++
              ")"
            Nothing ->
              "IMPOSSIBLE"
        _ ->
          "IMPOSSIBLE"

    writeFactTermEquation :: (String, Term) -> String
    writeFactTermEquation (var, term) =
      "$eq(" ++ var ++ ", " ++ (writeFactTerm term) ++ ")"

    writeFactTerm :: Term -> String
    writeFactTerm (Atom str) = "'" ++ str ++ "'"
    writeFactTerm (IntLiteral int) = show int
    writeFactTerm (Variable _) = "IMPOSSIBLE"

    makeFactGroupArguments :: [AST] -> String
    makeFactGroupArguments astList =
      astList
      |> inferArity
      |> makeVariables
      |> intersperse ", "
      |> join

    inferArity :: [AST] -> Int
    inferArity astList =
      case uncons astList of
        Just (ast, _) -> inferRelationArity ast
        Nothing -> 0

    makeVariables :: Int -> [String]
    makeVariables arity =
      [1..arity]
      |> map (\x -> "X" ++ show x)

writeAllRules :: [AST] -> String
writeAllRules rules =
  rules
  |> reverse
  |> groupByRelation
  |> map writeRuleGroup
  |> intersperse "\n"
  |> join

  where
    writeRuleGroup :: (String, [AST]) -> String
    writeRuleGroup (name, rules) =
      let
        args = makeRuleGroupArgs rules
        dummyVars = getDummyVars rules
        rulesWritten = rules |> map writeRule
      in
        "function " ++ name ++ "(" ++ args ++ ") {\n" ++
        (dummyVars |> map (\var ->
        "  var " ++ (writeVar var) ++ " = $lvar();\n") |> join) ++
        (if length rules == 1 then
          "  return " ++ (rulesWritten |> join) ++ ";\n"
        else
          "  return $or(\n" ++
          "    " ++ (rulesWritten |> intersperse ",\n    " |> join) ++ "\n" ++
          "  );\n"
        ) ++
        "}\n"

    writeRule :: AST -> String
    writeRule (Rule (Relation name headVars) body) =
      let
        bodyRelationsWritten = body |> map writeBodyRelation
      in
        (if length body == 1 then
          (bodyRelationsWritten |> join)
        else
          "$and(" ++ (bodyRelationsWritten |> intersperse ", " |> join) ++ ")"
        )
    writeRule _ = "IMPOSSIBLE"

    getDummyVars :: [AST] -> [Term]
    getDummyVars rules =
      let
        headVars = getHeadVars rules
        bodyVars = rules |> map getBodyVars |> flatten |> nub
      in
        bodyVars \\ headVars

    getHeadVars :: [AST] -> [Term]
    getHeadVars rules =
      case uncons rules of
        Just (Rule (Relation _ headVars) _, _) -> headVars
        _ -> []

    getBodyVars :: AST -> [Term]
    getBodyVars (Rule _ body) =
      body
      |> map (\(Relation _ terms) -> terms)
      |> flatten
      |> nub
    getBodyVars _ = []

    makeRuleGroupArgs :: [AST] -> String
    makeRuleGroupArgs rules =
      case uncons rules of
        Just (Rule (Relation _ headVars) _, rest) -> makeRuleArgs headVars
        _ -> ""

    makeRuleArgs :: [Term] -> String
    makeRuleArgs terms =
      terms
      |> map writeVar
      |> intersperse ", "
      |> join

    writeVar :: Term -> String
    writeVar (Variable x) = x
    writeVar _ = "IMPOSSIBLE"

    writeBodyRelation :: Relation -> String
    writeBodyRelation (Relation name vars) =
      name ++ "(" ++ (vars |> map writeVar |> intersperse ", " |> join) ++ ")"

writeEvaluationVariables :: [AST] -> String
writeEvaluationVariables evaluations =
  evaluations
  |> map extractEvaluationVariables
  |> flatten
  |> nub
  |> map writeVar
  |> intersperse "\n"
  |> join

  where
    extractEvaluationVariables :: AST -> [Term]
    extractEvaluationVariables ast =
      case ast of
        (Evaluation (Relation _ terms)) ->
          terms |> filter isVariable
        _ ->
          []

    writeVar :: Term -> String
    writeVar (Variable v) = "var " ++ v ++ " = $lvar();\n"
    writeVar _ = "IMPOSSIBLE"

writeAllEvaluations :: [AST] -> String
writeAllEvaluations evaluations =
  evaluations
  |> map writeEvaluation
  |> join

  where
    writeEvaluation :: AST -> String
    writeEvaluation (Evaluation (Relation name terms)) =
      let
        args = terms |> map writeTerm |> intersperse "," |> join
        vars = terms |> filter isVariable |> map writeTerm
        varsAsStrings = vars |> map wrapWithQuotes |> intersperse "," |> join
        rawVars = vars |> intersperse "," |> join
        title = writeReportTitle (Relation name terms)
      in
        "$report(" ++
        title ++ ", " ++
        "[" ++ varsAsStrings ++ "], " ++
        "$logic.run(" ++ name ++ "(" ++ args ++ "), [" ++ rawVars ++ "])" ++
        ");\n"
    writeEvaluation _ = "IMPOSSIBLE"

    writeTerm :: Term -> String
    writeTerm (Atom str) = "'" ++ str ++ "'"
    writeTerm (IntLiteral int) = show int
    writeTerm (Variable var) = var

    writeReportTitle :: Relation -> String
    writeReportTitle (Relation name terms) =
      "'" ++ name ++ "(" ++ (terms |> map (\term ->
        case term of
          Atom str -> str
          IntLiteral int -> show int
          Variable var -> var
      ) |> intersperse "," |> join) ++
      ")'"

    wrapWithQuotes :: String -> String
    wrapWithQuotes x = "'" ++ x ++ "'"

flatten :: [[a]] -> [a]
flatten list = list >>= id

join :: [String] -> String
join = foldl (++) ""