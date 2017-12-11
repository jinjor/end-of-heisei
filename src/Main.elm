module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time, every, millisecond)
import Time.DateTime as DateTime exposing (DateTime, DateTimeDelta)


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    Maybe Time


type alias Msg =
    Time


init : ( Model, Cmd Msg )
init =
    ( Nothing, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update time model =
    ( Just time
    , Cmd.none
    )


endOfHeisei : DateTime
endOfHeisei =
    case DateTime.fromISO8601 "2019-05-01T00:00:00+09:00" of
        Ok dt ->
            dt

        _ ->
            Debug.crash "invalid format"


view : Model -> Html Msg
view model =
    case model of
        Just current ->
            let
                currentDateTime =
                    DateTime.fromTimestamp current

                ( num, unit ) =
                    formatDelta currentDateTime endOfHeisei
            in
            div
                [ class "container" ]
                [ div [ class "head" ] [ text "平成終了まで" ]
                , div [ class "main" ]
                    [ span [ class "left" ] [ text "あと" ]
                    , span [ class "num" ] [ text (toString num) ]
                    , span [ class "unit" ] [ text unit ]
                    ]
                ]

        Nothing ->
            div
                [ class "container"
                , attribute "aria-hidden" "true"
                ]
                []


formatDelta : DateTime -> DateTime -> ( Int, String )
formatDelta from to =
    case DateTime.compare from to of
        GT ->
            ( 0, "秒" )

        _ ->
            let
                delta =
                    DateTime.delta to from
            in
            if delta.days >= 1 then
                ( delta.days + 1, "日" )
            else if delta.hours >= 1 then
                ( delta.hours + 1, "時間" )
            else if delta.minutes >= 1 then
                ( delta.minutes + 1, "分" )
            else
                ( delta.seconds + 1, "秒" )


subscriptions : Model -> Sub Msg
subscriptions _ =
    every (20 * millisecond) identity
