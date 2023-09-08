module Stlc.Syntax
  where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)




newtype Symbol = Symbol String

derive instance genericSymbol :: Generic Symbol _

derive newtype instance showSymbol :: Show Symbol

data Type
  = TyAtom Symbol
  | TyFunction Type Type
  | TySum Type Type
  | TyProduct Type Type

derive instance genericType :: Generic Type _

instance Show Type where
  show t = genericShow t


-- Note: there's no way to actually get values of atomic types;
-- this isn't necessary for pure logic (I suspect later on when
-- they write the interpreter they'll have to change this, since
-- you can't interpret much without values).
data Term
  = TmVariable Symbol
  | TmProduct Term Term
  | TmFirst Term
  | TmSecond Term
  | TmLeft Term
  | TmRight Term
  -- match f g x :: Sum a b -> c, where f :: a -> c and g :: b -> c
  | TmMatch Term Term Term
  | TmApplication Term Term
  -- \x y z . term .... we may change this to force currying
  | TmAbstraction (NonEmptyArray Symbol) Term

derive instance genericTerm :: Generic Term _

instance Show Term where
  show t = genericShow t

data Ast
  = AtomDecl Symbol
  | TermDef Symbol Type Term


derive instance genericAst :: Generic Ast _

instance Show Ast where
  show = genericShow