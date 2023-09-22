module Cuderia.Syntax.Token
  ( Identifier (..),
    SExpr (..),
    Construct (..),
  )
where

import Data.Text qualified as T

newtype Identifier = Identifier T.Text

data Construct
  = Var Identifier
  | SExpr SExpr

data SExpr
  = App Identifier [Construct]