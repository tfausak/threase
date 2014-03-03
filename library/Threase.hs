-- | Plays <http://asherv.com/threes/ Threes> with ease.
module Threase (module Threase) where

import           Threase.Board     as Threase
import           Threase.Direction as Threase
import           Threase.Game      as Threase
import           Threase.Tile      as Threase hiding (render, score)
import           Threase.Vector    as Threase hiding (canShift, render, score,
                                               shift)
-- import X.X.X as Threase
