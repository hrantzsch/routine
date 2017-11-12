module Components where

import Control.Monad.Eff (Eff)

import React (Event, EventHandlerContext, ReactElement, ReactProps, ReactRefs, ReactState, Read, Write)
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
