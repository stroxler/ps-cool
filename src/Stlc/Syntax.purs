module Stlc.Syntax where

import Prelude


newtype Type = Type
  { name :: String
  , other :: Unit
  }