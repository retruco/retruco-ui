module Data exposing (..)

import Array
import Dict
import Set
import Types exposing (Data, DataProxy, DataWithId, DataWithIds, StatementWrapper(..))


addToData : StatementWrapper -> DataProxy a -> DataProxy a
addToData statementWrapper data =
    case statementWrapper of
        CardWrapper card ->
            { data
                | cards = Dict.insert card.id card data.cards
            }

        PropertyWrapper property ->
            { data
                | properties = Dict.insert property.id property data.properties
            }

        TypedValueWrapper typedValue ->
            { data
                | values = Dict.insert typedValue.id typedValue data.values
            }


initData : Data
initData =
    { ballots = Dict.empty
    , cards = Dict.empty
    , collections = Dict.empty
    , properties = Dict.empty
    , users = Dict.empty
    , values = Dict.empty
    }


initDataWithId : DataWithId
initDataWithId =
    { ballots = Dict.empty
    , cards = Dict.empty
    , collections = Dict.empty
    , id = ""
    , properties = Dict.empty
    , users = Dict.empty
    , values = Dict.empty
    }


initDataWithIds : DataWithIds
initDataWithIds =
    { ballots = Dict.empty
    , cards = Dict.empty
    , collections = Dict.empty
    , ids = Array.empty
    , properties = Dict.empty
    , users = Dict.empty
    , values = Dict.empty
    }


mergeData : DataProxy a -> DataProxy b -> DataProxy b
mergeData new old =
    let
        -- Remove old ballots that are not present in new.
        filterObsoleteBallotIds =
            List.filterMap
                (\{ ballotId } ->
                    let
                        hasNewBallot =
                            case Dict.get ballotId new.ballots of
                                Just newBallot ->
                                    not newBallot.deleted

                                Nothing ->
                                    False
                    in
                        if hasNewBallot then
                            Nothing
                        else
                            case Dict.get ballotId old.ballots of
                                Just oldBallot ->
                                    if oldBallot.deleted then
                                        Just ballotId
                                    else
                                        Nothing

                                Nothing ->
                                    Just ballotId
                )

        obsoleteBallotIds =
            Set.fromList <|
                List.concat
                    [ Dict.values new.cards |> filterObsoleteBallotIds
                    , Dict.values new.properties |> filterObsoleteBallotIds
                    , Dict.values new.values |> filterObsoleteBallotIds
                    ]

        oldBallots =
            Dict.filter
                (\ballotId _ -> not <| Set.member ballotId obsoleteBallotIds)
                old.ballots
    in
        { old
            | ballots = Dict.union new.ballots oldBallots
            , cards = Dict.union new.cards old.cards
            , collections = Dict.union new.collections old.collections
            , properties = Dict.union new.properties old.properties
            , users = Dict.union new.users old.users
            , values = Dict.union new.values old.values
        }
