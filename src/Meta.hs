module Meta
  ( Meta(..)
  , MetaW
  ) where

import Control.Monad.Writer (Writer)

data Meta
  = Meta { line :: Integer
         , errors :: [String] }
  | Empty
  deriving (Show, Eq)

instance Monoid Meta where
  mempty = Empty

instance Semigroup Meta where
  (<>) = const

type MetaW a = Writer Meta a
