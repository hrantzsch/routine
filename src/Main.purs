module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
{-- import Data.Array ((..), length, modifyAt, zipWith) --}
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Foreign (ForeignError, readString, toForeign)
import Data.Foreign.Index (index)
import Data.Int (fromString)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (fromJust, fromMaybe)
import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Partial.Unsafe (unsafePartial)
import React (ReactClass, ReadWrite, ReactState, Event, ReactThis,
              createFactory, readState, spec, createClass, writeState)
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)

import Data.Routine (Errors, Routine(..), validateRoutine)
import Components (routineForm)

routine1 :: Routine
routine1 = Routine { title: "coffee", period: "3", start: "heute", code: "AABBCC" }

newtype AppState = AppState
  { routine :: Routine
  , errors :: Errors
  }

initialState :: AppState
initialState = AppState
  { routine: routine1
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
    let newRoutine = update s

    log "Running validators"
    case validateRoutine newRoutine of
      Left errors -> writeState ctx (AppState { routine: newRoutine, errors: errors })
      Right _ -> writeState ctx (AppState { routine: newRoutine, errors: [] })

routineList :: forall props. ReactClass props
routineList = createClass $ spec initialState \ctx -> do
  AppState { routine: Routine routine@{ }, errors } <- readState ctx

  let renderValidationError err = D.li' [ D.text err ]

      renderValidationErrors [] = []
      renderValidationErrors xs =
        [ D.div [ P.className "alert alert-danger" ]
                [ D.ul' (map renderValidationError xs) ]
        ]

      updateTitle t = Routine $ routine { title = t }
      updatePeriod p = Routine $ routine { period = p }
      updateStart s = Routine $ routine { start = s }

  pure $
    D.div [ P.className "container" ]
          [ D.div [ P.className "row" ]
                  (renderValidationErrors errors)
          , routineForm routine
              (updateAppState ctx updateTitle)
              (updateAppState ctx updatePeriod)
              (updateAppState ctx updateStart)
          ]

main :: forall e. Eff (console :: CONSOLE, dom:: DOM | e) Unit
main = void do
  log "Rendering routines component"
  let component = D.div [] [ createFactory routineList unit ]
  doc <- window >>= document
  ctr <- getElementById (ElementId "main") (documentToNonElementParentNode (htmlDocumentToDocument doc))
  render component (unsafePartial fromJust ctr)
