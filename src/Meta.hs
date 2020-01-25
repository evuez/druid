module Meta
  ( Meta(..)
  ) where

data Meta
  = Meta { line :: Integer
         , errors :: [String] }
  | Empty
  deriving (Show, Eq)

instance Monoid Meta where
  mempty = Empty

instance Semigroup Meta where
  (<>) = const
