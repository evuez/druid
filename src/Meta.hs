module Meta (Meta(..)) where

data Meta
  = Meta { line :: Integer }
  | Empty
  deriving (Show, Eq)

instance Monoid Meta where
  mempty = Empty

instance Semigroup Meta where
  (<>) = const
