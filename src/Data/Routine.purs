module Data.Routine where

import Prelude
import Data.Show

type Days = Int
type Date = String
type Code = String

data Routine = Routine
    { title :: String
    , period :: Days
    , start :: Date
    , code :: Code
    }

instance showRoutine :: Show Routine where
    show (Routine {title: t, period: p, start: s, code: c }) = t
