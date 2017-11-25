module Components where

import Prelude
import Control.Monad.Eff (Eff)

import React (Event, ReactElement, ReactProps, ReactRefs, ReactState, Read, Write)
import React.DOM as D
import React.DOM.Props as P

import Data.Routine (Routine(..))

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

{-- routineForm  :: forall props eff --}
{--     . Routine --}
{--    -> (Event -> Eff ( props :: ReactProps --}
{--                     , refs :: ReactRefs ( read :: Read) --}
{--                     , state :: ReactState ( read :: Read , write :: Write) --}
{--                     | eff --}
{--                     ) props --}
{--                     ) --}
{--    -> (Event -> Eff ( props :: ReactProps --}
{--                     , refs :: ReactRefs ( read :: Read) --}
{--                     , state :: ReactState ( read :: Read , write :: Write) --}
{--                     | eff --}
{--                     ) props --}
{--                     ) --}
{--    -> (Event -> Eff ( props :: ReactProps --}
{--                     , refs :: ReactRefs ( read :: Read) --}
{--                     , state :: ReactState ( read :: Read , write :: Write) --}
{--                     | eff --}
{--                     ) props --}
{--                     ) --}
{--     -> ReactElement --}
routineForm r onTitleChanged onPeriodChanged onStartChanged =
    D.div [ P.className "row" ]
        [ D.form [ P.className "form-horizontal" ] $
            [ D.h3' [ D.text "Basic Information" ]
            , formField "Title" "title" r.title onTitleChanged
            , formField "Period"  "days until repetition" r.period onPeriodChanged
            , formField "Starting" "start date" r.start onStartChanged
            , D.div [ P.className "form-group" ]
                [ D.label [ P.className "col-sm-2 control-label" ]
                    [ D.text "Code" ]
                ]
            ]
        ]

routineTable :: Array Routine -> ReactElement
routineTable routines =
    D.div [ P.className "container" ]
        [ D.table [ P.className "table table-sm" ]
            [ D.thead' [ D.tr' [ D.th' [ D.text "title" ]
                               , D.th' [ D.text "period" ]
                               , D.th' [ D.text "start" ]
                               , D.th' [ D.text "code" ] ] ]
            , D.tbody' $ map renderRow routines
            ]
        ]
    where renderRow (Routine r) = D.tr' [ D.td' [ D.text r.title ]
                                        , D.td' [ D.text r.period ]
                                        , D.td' [ D.text r.start ]
                                        , D.td' [ D.text r.code ] ]

renderValidationErrors :: Array String -> ReactElement
renderValidationErrors [] = D.div [] []
renderValidationErrors errors =
  D.div [ P.className "alert alert-danger" ]
          [ D.ul' (map renderValidationError errors) ]
    where renderValidationError err = D.li' [ D.text err ]
