{-# LANGUAGE OverloadedStrings #-}

module Cuderia.Syntax.Unparser (unparse, unparseProgram) where

import Cuderia.Syntax.Parser
import Data.Text qualified as T

unparseIdentifier :: Identifier -> T.Text
unparseIdentifier (Identifier i) = i

unparseConstruct :: Construct -> T.Text
unparseConstruct (Var v) = unparseIdentifier v
unparseConstruct (Expr e) = unparseSExpr e
unparseConstruct (Integer i) = T.pack $ show i
unparseConstruct (String s) = "\"" <> s <> "\""
unparseConstruct (Slot i) = T.pack $ "'" ++ show i

unparseBinding :: (Identifier, Construct) -> T.Text
unparseBinding (Identifier name, c) = "(" <> name <> " " <> unparseConstruct c <> ")"

unparseSExpr :: SExpr -> T.Text
unparseSExpr (Apply constructs) = "(" <> T.intercalate " " (map unparseConstruct constructs) <> ")"
unparseSExpr (Let bindings body) = "(let (" <> T.intercalate " " (map unparseBinding bindings) <> ") " <> unparseSExpr body <> ")"
unparseSExpr (Lambda args body) = "(lambda (" <> T.intercalate " " (map unparseIdentifier args) <> ") " <> unparseSExpr body <> ")"

unparseExprs :: T.Text -> [SExpr] -> T.Text
unparseExprs joiner exprs = T.intercalate joiner $ map unparseSExpr exprs

unparse :: [SExpr] -> T.Text
unparse = unparseExprs "\n"

unparseProgram :: Program -> T.Text
unparseProgram program = T.intercalate "\n" (map unparseSExpr $ exprs program)