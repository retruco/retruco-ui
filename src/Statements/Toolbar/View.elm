module Statements.Toolbar.View exposing (..)

import Html exposing (..)
import Statements.Toolbar.Types exposing (..)
import Statements.ViewsHelpers exposing (viewStatementRatingToolbar)
import Types exposing (..)


view : Model (Statement b) -> Html Msg
view model =
    viewStatementRatingToolbar model.language (ForSelf << Rate) (ForSelf <| Trash) model.data model.statement
