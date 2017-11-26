module Main where

import Prelude

import Components (renderRoutineForm, renderRoutineList, renderValidationErrors)
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
import Data.Routine (Errors, Routine(..), routine, validateRoutine)
import Partial.Unsafe (unsafePartial)
import React (Event, ReactClass, ReactState, ReactThis, ReadWrite, createClass, createFactory, readState, spec, writeState)
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)

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

updateNewRoutineForm :: forall props eff
   . ReactThis props AppState
  -> (String -> Routine)
  -> Event
  -> Eff ( state :: ReactState ReadWrite | eff) Unit
updateNewRoutineForm ctx update event =
  for_ (valueOf event) \value -> do
    let formData = update value
    AppState { routines: routines } <- readState ctx

    case validateRoutine formData of
      Left errors -> writeState ctx $ AppState { routines: routines, newRoutine: formData, errors: errors }
      Right _     -> writeState ctx $ AppState { routines: routines, newRoutine: formData, errors: [] }

submitNewRoutineForm :: forall props eff
   . ReactThis props AppState
  -> Eff ( state :: ReactState ReadWrite | eff) Unit
submitNewRoutineForm ctx = void do
    AppState { routines: routines, newRoutine: formData } <- readState ctx

    case validateRoutine formData of
      Left errors -> writeState ctx $
          AppState { routines: routines, newRoutine: formData, errors: errors }
      Right _     -> writeState ctx $
          AppState { routines: [formData] <> routines, newRoutine: emptyRoutine, errors: [] }

routineList :: forall props. ReactClass props
routineList = createClass $ spec initialState \ctx -> do
    AppState { routines: routines, newRoutine: newRoutine, errors } <- readState ctx

    pure $
        D.div [ P.className "column" ]
          [ renderRoutineForm newRoutine (updateNewRoutineForm ctx) (submitNewRoutineForm ctx)
          , renderValidationErrors errors
          , renderRoutineList routines
          ]

main :: forall e. Eff (console :: CONSOLE, dom:: DOM | e) Unit
main = void do
  log "Rendering routines component"
  let component = D.div [] [ createFactory routineList unit ]
  doc <- window >>= document
  ctr <- getElementById (ElementId "main") (documentToNonElementParentNode (htmlDocumentToDocument doc))
  render component (unsafePartial fromJust ctr)
