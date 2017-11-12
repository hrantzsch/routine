module Data.Routine where

import Prelude
import Data.Either (Either( .. ))

type Errors = Array String

type Days = Int
type Date = String
type Code = String

newtype Routine = Routine
    { title :: String
    , period :: Days
    , start :: Date
    , code :: Code
    }

routine :: String -> Days -> Date -> Code -> Routine
routine title period start code = Routine { title, period, start, code }

instance showRoutine :: Show Routine where
    show (Routine {title: t, period: p, start: s, code: c }) = t

validateRoutine :: Routine -> Either Errors Routine
validateRoutine r = Right r
