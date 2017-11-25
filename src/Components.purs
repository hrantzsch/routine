module Components where

import Prelude

import React (ReactElement)
import React.DOM as D
import React.DOM.Props as P

import Data.Routine (Routine(..))

renderRoutineForm :: Routine -> ReactElement
renderRoutineForm routine =
    D.div [ P.className "container" ] [ routineForm routine ]

    where
        routineInput value placeholder = D.div [ P.className "col" ]
            [ D.input [ P._type "text", P.className "form-control",
                        P.placeholder placeholder]
              []
            ]

        routineForm (Routine r) =
            D.form [ P.className "form-row" ]
            [ routineInput r.title "title"
            , routineInput r.period "period"
            , routineInput r.start "start"
            ]

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
