module Components where

import Prelude
import Control.Monad.Eff (Eff)

import React (Event, ReactElement, ReactProps, ReactRefs, ReactState, Read, ReadWrite)
import React.DOM as D
import React.DOM.Props as P

import Data.Routine (Routine(..))

type StateEffect = forall eff. Eff ( props :: ReactProps
                                   , refs :: ReactRefs ( read :: Read )
                                   , state :: ReactState ReadWrite
                                   | eff) Unit

renderRoutineForm :: Routine
                     -> ((String -> Routine) -> Event -> StateEffect)
                     -> StateEffect
                     -> ReactElement
renderRoutineForm routine updateForm submitForm =
    D.div [ P.className "container" ] [ routineForm routine ]
    where
        routineForm (Routine r) =
            D.div [ P.className "form-row" ]
            [ routineInput r.title  "title"  (\s -> Routine $ r { title  = s } )
            , routineInput r.period "period" (\s -> Routine $ r { period = s } )
            , routineInput r.start  "start"  (\s -> Routine $ r { start  = s } )
            , D.button [ P.className "btn-light", P.onClick (const submitForm) ] [ D.text "+" ]
            ]

        routineInput value placeholder updateField = D.div [ P.className "col" ]
            [ D.input [ P.className "form-control"
                      , P.placeholder placeholder
                      , P.value value
                      , P.onKeyUp submitOnReturnKey
                      , P.onChange $ updateForm updateField ]
              []
            ]

        submitOnReturnKey { keyCode: 13 } = submitForm
        submitOnReturnKey { keyCode: _  } = pure unit

renderRoutineList :: Array Routine -> ReactElement
renderRoutineList routines =
    D.div [ P.className "container" ]
        [ D.table [ P.className "table table-sm" ]
            [ D.thead' [ D.tr' [ D.th' [ D.text "title" ]
                               , D.th' [ D.text "period" ]
                               , D.th' [ D.text "start" ]
                               , D.th' [ D.text "code" ] ] ]
            , D.tbody' $ map renderRoutine routines
            ]
        ]
    where
        renderRoutine (Routine r) = D.tr' [ D.td' [ D.text r.title ]
                                          , D.td' [ D.text r.period ]
                                          , D.td' [ D.text r.start ]
                                          , D.td' [ D.text r.code ]
                                          ]

renderValidationErrors :: Array String -> ReactElement
renderValidationErrors [] = D.div [] []
renderValidationErrors errors =
  D.div [ P.className "alert alert-danger" ]
          [ D.ul' (map renderValidationError errors) ]
    where renderValidationError err = D.li' [ D.text err ]
