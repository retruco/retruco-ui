module Statements.Toolbar.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Statements.Toolbar.Types exposing (..)
import Statements.ViewsHelpers exposing (viewStatementRatingToolbar, viewStatementSocialToolbar)
import Types exposing (..)


view : Model (Statement b) -> Html Msg
view model =
    div [ class "d-flex justify-content-between" ]
        [ viewStatementRatingToolbar model.language (ForSelf << Rate) (ForSelf <| Trash) model.data model.statement
        , viewStatementSocialToolbar
            model.language
            (ForSelf << ShareOnFacebook)
            (ForSelf << ShareOnGooglePlus)
            (ForSelf << ShareOnLinkedIn)
            (ForSelf << ShareOnTwitter)
            model.data
            model.statement
        ]
