module Stlc.Syntax
  where

import Data.List.Lazy.Types (NonEmptyList)
import Data.Tuple (Tuple)




newtype Symbol = Symbol String

data Type
  = TyAtom Symbol
  | TyFunction Type Type
  | TySum Type Type
  | TyProduct Type Type


data Term
  -- TmSymbol covers both atoms and variables because we can't tell them apart syntactically.
  = TmSymbol Symbol
  | TmProduct Term Term
  | TmFirst Term
  | TmSecond Term
  | TmLeft Term
  | TmRight Term
  -- match f g x :: Sum a b -> c, where f :: a -> c and g :: b -> c
  | TmMatch Term Term Term
  | TmApplication Term Term
  -- \x y z . term .... we may change this to force currying
  | TmAbstraction (NonEmptyList Symbol) Term


data AST
  = AtomDecl Symbol
  | TermDef Symbol Type Term