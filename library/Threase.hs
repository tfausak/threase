-- | Plays <http://asherv.com/threes/ Threes> with ease.
module Threase (module Threase) where

import           Threase.Board     as Threase hiding (render)
import           Threase.Direction as Threase hiding (render)
import           Threase.Game      as Threase
import           Threase.Tile      as Threase hiding (render, score)
import           Threase.Vector    as Threase hiding (canShift, render, score,
                                               shift, shiftWith)
-- HASKELETON: import qualified New.Module as Threase
