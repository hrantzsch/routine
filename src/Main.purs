module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Foreign (ForeignError, readString, toForeign)
import Data.Foreign.Index (index)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import React (ReactClass, ReadWrite, ReactState, Event, ReactThis, createFactory, readState, spec, createClass, writeState)
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)

import Components (routineForm, routineTable, renderValidationErrors)
import Data.Routine (Errors, Routine(..), routine, validateRoutine)

routine1 :: Routine
routine1 = Routine { title: "Kaffee trinken", period: "3", start: "heute", code: "AABBCC" }
routine2 :: Routine
routine2 = Routine { title: "Pflanzen gießen", period: "2", start: "gestern", code: "AABBCC" }

emptyRoutine :: Routine
emptyRoutine = routine "" "" "" "1234"

newtype AppState = AppState
  { routines :: Array Routine
  , newRoutine :: Routine
  , errors :: Errors
  }

initialState :: AppState
initialState = AppState
  { routines: [ routine1, routine2 ]
  , newRoutine: emptyRoutine
  , errors: []
  }

valueOf :: Event -> Either (NonEmptyList ForeignError) String
valueOf e = runExcept do
  target <- index (toForeign e) "target"
  value <- index target "value"
  readString value

updateAppState :: forall props eff
   . ReactThis props AppState
  -> (String -> Routine)
  -> Event
  -> Eff ( console :: CONSOLE , state :: ReactState ReadWrite | eff) Unit
updateAppState ctx update e =
  for_ (valueOf e) \s -> do
    AppState { routines: rs, newRoutine: nr, errors: er } <- readState ctx
    let newRoutine = update s

    log "Running validators"
    case validateRoutine newRoutine of
      Left errors -> writeState ctx $ AppState { routines: rs, newRoutine: newRoutine, errors: errors }
      Right _     -> writeState ctx $ AppState { routines: rs, newRoutine: newRoutine, errors: [] }

routineList :: forall props. ReactClass props
routineList = createClass $ spec initialState \ctx -> do
    AppState { routines: routines, newRoutine: Routine newRoutine, errors } <- readState ctx

    let updateTitle t = Routine $ newRoutine { title = t }
        updatePeriod p = Routine $ newRoutine { period = p }
        updateStart s = Routine $ newRoutine { start = s }

    pure $
        D.div [ P.className "column" ]
          [ renderValidationErrors errors
          , routineForm newRoutine
              (updateAppState ctx updateTitle)
              (updateAppState ctx updatePeriod)
              (updateAppState ctx updateStart)
              {-- (\_ -> do readState ctx >>= updateErrors >>> writeState ctx) --}
              {-- (\_ -> do readState ctx >>= updateErrors >>> writeState ctx) --}
              {-- (\_ -> do readState ctx >>= updateErrors >>> writeState ctx) --}
          , routineTable routines
          ]

main :: forall e. Eff (console :: CONSOLE, dom:: DOM | e) Unit
main = void do
  log "Rendering routines component"
  let component = D.div [] [ createFactory routineList unit ]
  doc <- window >>= document
  ctr <- getElementById (ElementId "main") (documentToNonElementParentNode (htmlDocumentToDocument doc))
  render component (unsafePartial fromJust ctr)
