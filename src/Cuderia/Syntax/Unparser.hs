{-# LANGUAGE OverloadedStrings #-}

module Cuderia.Syntax.Unparser (unparse) where

import Cuderia.Syntax.Token
import Data.Text qualified as T

unparseIdentifier :: Identifier -> T.Text
unparseIdentifier (Identifier i) = i

unparseConstruct :: Construct -> T.Text
unparseConstruct (Var v) = unparseIdentifier v
unparseConstruct (SExpr e) = unparseOne e

unparseOne :: SExpr -> T.Text
unparseOne (App id args) = "(" <> (unparseIdentifier id) <> " " <> (T.intercalate " " $ map unparseConstruct args) <> ")"

unparseExprs :: T.Text -> [SExpr] -> T.Text
unparseExprs joiner exprs = T.intercalate joiner $ map unparseOne exprs

unparse :: [SExpr] -> T.Text
unparse = unparseExprs "\n"