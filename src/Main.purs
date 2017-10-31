module Main where

import Prelude
import Math (sqrt)
import Control.Monad.Eff
import Control.Monad.Eff.Console

diagonal w h = sqrt (w * w + h * h)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = logShow $ diagonal 3.0 4.0
