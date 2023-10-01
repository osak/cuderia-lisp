{-# LANGUAGE OverloadedStrings #-}

module Cuderia.Syntax.Unparser (unparse, unparseProgram) where

import Cuderia.Syntax.Parser
import Data.Text qualified as T

unparseIdentifier :: Identifier -> T.Text
unparseIdentifier (Identifier i) = i

unparseTerm :: Term -> T.Text
unparseTerm (Var v) = unparseIdentifier v
unparseTerm (Expr e) = unparseSExpr e
unparseTerm (Integer i) = T.pack $ show i
unparseTerm (String s) = "\"" <> s <> "\""
unparseTerm (Slot i) = T.pack $ "'" ++ show i

unparseBinding :: (Identifier, Term) -> T.Text
unparseBinding (Identifier name, c) = "(" <> name <> " " <> unparseTerm c <> ")"

unparseSExpr :: SExpr -> T.Text
unparseSExpr (Apply constructs) = "(" <> T.intercalate " " (map unparseTerm constructs) <> ")"
unparseSExpr (Let bindings body) = "(let (" <> T.intercalate " " (map unparseBinding bindings) <> ") " <> unparseSExpr body <> ")"
unparseSExpr (Lambda args body) = "(lambda (" <> T.intercalate " " (map unparseIdentifier args) <> ") " <> unparseSExpr body <> ")"
unparseSExpr (If cond body1 body2) = "(if " <> unparseTerm cond <> " " <> unparseTerm body1 <> " " <> unparseTerm body2 <> ")"

unparseExprs :: T.Text -> [SExpr] -> T.Text
unparseExprs joiner exprs = T.intercalate joiner $ map unparseSExpr exprs

unparse :: [SExpr] -> T.Text
unparse = unparseExprs "\n"

unparseProgram :: Program -> T.Text
unparseProgram program = T.intercalate "\n" (map unparseSExpr $ exprs program)