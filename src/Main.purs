module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)

import Data.List

import Data.Routine

myRoutines :: List Routine
myRoutines =
    Routine { title: "coffee", period: 3, start: "heute", code: "AABBCC" }
    : singleton (Routine { title: "certs", period: 3, start: "heute", code: "AABBCC" })

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = logShow $ "hello world"
