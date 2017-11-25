module Data.Routine where

import Prelude

import Data.Either (Either(..))
import Data.Validation.Semigroup (V, unV, invalid)

type Errors = Array String

type Title = String
type Days = String
type Date = String
type Code = String

newtype Routine = Routine
    { title :: Title
    , period :: Days
    , start :: Date
    , code :: Code
    }

routine :: Title -> Days -> Date -> Code -> Routine
routine title period start code = Routine { title, period, start, code }

instance showRoutine :: Show Routine where
    show (Routine {title: t, period: p, start: s, code: c }) = t

nonEmpty :: String -> String -> V Errors Unit
nonEmpty field "" = invalid ["Field '" <> field <> "' must not be empty"]
nonEmpty _     _  = pure unit

validateTitle :: Title -> V Errors Title
validateTitle title = nonEmpty "title" title *> pure title

validatePeriod :: Days -> V Errors Days
validatePeriod period= nonEmpty "period" period *> pure period

validateStart :: Date -> V Errors Date
validateStart start = nonEmpty "start" start *> pure start

validateCode :: Code -> V Errors Code
validateCode code= nonEmpty "code" code*> pure code

validateRoutine' :: Routine -> V Errors Routine
validateRoutine' (Routine r) = routine <$> validateTitle r.title
                                       <*> validatePeriod r.period
                                       <*> validateStart r.start
                                       <*> validateCode r.code

validateRoutine :: Routine -> Either Errors Routine
validateRoutine r = unV Left Right $ validateRoutine' r
