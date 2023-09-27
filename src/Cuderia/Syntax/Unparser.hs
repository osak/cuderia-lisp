{-# LANGUAGE OverloadedStrings #-}

module Cuderia.Syntax.Unparser (unparse) where

import Cuderia.Syntax.Parser
import qualified Data.Text as T

unparseIdentifier :: Identifier -> T.Text
unparseIdentifier (Identifier i) = i

unparseConstruct :: Construct -> T.Text
unparseConstruct (Var v) = unparseIdentifier v
unparseConstruct (Expr e) = unparseSExpr e
unparseConstruct (Integer i) = T.pack $ show i
unparseConstruct (String s) = "\"" <> s <> "\""
unparseConstruct (Slot i) = T.pack $ "'" ++ show i

unparseSExpr :: SExpr -> T.Text
unparseSExpr (SExpr constructs) = "(" <> T.intercalate " " (map unparseConstruct constructs) <> ")"

unparseExprs :: T.Text -> [SExpr] -> T.Text
unparseExprs joiner exprs = T.intercalate joiner $ map unparseSExpr exprs

unparse :: [SExpr] -> T.Text
unparse = unparseExprs "\n"