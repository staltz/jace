module Transformer (transform) where

import Parser (AST(..), Term(..), Relation(..))
import Data.List (sortBy)

(|>) = flip ($)

transform :: [AST] -> [AST]
transform astList =
  astList
  |> placeEvaluationsLast

evaluationsLast :: AST -> AST -> Ordering
evaluationsLast (Evaluation _) (Evaluation _) = EQ
evaluationsLast _ (Evaluation _) = LT
evaluationsLast (Evaluation _) _ = GT
evaluationsLast _ _ = EQ

placeEvaluationsLast :: [AST] -> [AST]
placeEvaluationsLast astList = sortBy evaluationsLast astList
