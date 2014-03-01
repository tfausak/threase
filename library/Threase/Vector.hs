-- | Types and tools for working with linear arrays of tiles.
module Threase.Vector (Vector (..), shift) where

import           Data.Monoid  ((<>))
import           Threase.Tile (Tile, add, canAdd)

-- | A row or column of tiles.
data Vector = Vector
    { tiles :: [Maybe Tile] -- ^ The tiles in this row or column.
    } deriving (Eq, Show)

{- | Simulate a swipe and try to shift this vector. Moves tiles toward the head
of the list. -}
shift :: Vector -- ^ The vector.
    -> Vector -- ^ The shifted vector.
shift v = go (tiles v)
  where
    go (Nothing : rest) = Vector (rest <> [Nothing])
    go (Just a : Just b : rest) = if canAdd a b
        then Vector (Just (add a b) : rest <> [Nothing])
        else v
    go _ = v
