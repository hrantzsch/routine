module Components where

import Prelude
import Control.Monad.Eff (Eff)

import React (Event, ReactElement, ReactProps, ReactRefs, ReactState, Read, Write)
import React.DOM as D
import React.DOM.Props as P

formField :: forall props eff
    . String
   -> String
   -> String
   -> (Event -> Eff ( props :: ReactProps
                    , refs :: ReactRefs ( read :: Read)
                    , state :: ReactState ( read :: Read , write :: Write)
                    | eff
                    ) props
      )
   -> ReactElement
formField name hint value update =
    D.div [ P.className "form-group" ]
          [ D.label [ P.className "col-sm-2 control-label" ]
                    [ D.text name ]
          , D.div [ P.className "col-sm-3" ]
                  [ D.input [ P._type "text"
                            , P.className "form-control"
                            , P.placeholder hint
                            , P.value value
                            , P.onChange update
                            ] []
                  ]
          ]

routineTable routine =
    D.div [ P.className "container" ]
        [ D.table [ P.className "table table-sm" ]
            [ D.thead' [ D.tr'
                [ D.th' [ D.text "title" ]
                , D.th' [ D.text "period" ]
                , D.th' [ D.text "start" ]
                , D.th' [ D.text "code" ]
                ]
            ]
            , D.tbody'
                [ D.tr'
                    [ D.td' [ D.text routine.title ]
                    , D.td' [ D.text routine.period ]
                    , D.td' [ D.text routine.start ]
                    , D.td' [ D.text routine.code ]
                    ]
                ]
            , D.tbody'
                [ D.tr'
                    [ D.td' [ D.text routine.title ]
                    , D.td' [ D.text routine.period ]
                    , D.td' [ D.text routine.start ]
                    , D.td' [ D.text routine.code ]
                    ]
                ]
            ]
        ]

routineForm routine onTitleChanged onPeriodChanged onStartChanged =
    D.div [ P.className "row" ]
        [ D.form [ P.className "form-horizontal" ] $
            [ D.h3' [ D.text "Basic Information" ]
            , formField "Title" "title" routine.title onTitleChanged
            , formField "Period"  "days until repetition" routine.period onPeriodChanged
            , formField "Starting" "start date" routine.start onStartChanged
            , D.div [ P.className "form-group" ]
                [ D.label [ P.className "col-sm-2 control-label" ]
                    [ D.text "Code" ]
                ]
            ]
        ]
