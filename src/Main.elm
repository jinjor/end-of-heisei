module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time, every, millisecond)
import Time.DateTime as DateTime exposing (DateTime, DateTimeDelta)


type alias Flags =
    Int


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { offset : Int
    , current : Maybe Time
    }


type Msg
    = Tick Time


init : Flags -> ( Model, Cmd Msg )
init offset =
    ( Model offset Nothing, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update (Tick time) model =
    ( { model
        | current = Just time
      }
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
    case model.current of
        Just current ->
            let
                currentDateTime =
                    DateTime.fromTimestamp current
                        |> DateTime.addMinutes -model.offset

                delta =
                    DateTime.delta endOfHeisei currentDateTime

                ( num, unit ) =
                    formatDelta delta
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
                [ class "container", attribute "aria-hidden" "true" ]
                []


formatDelta : DateTimeDelta -> ( Int, String )
formatDelta delta =
    if delta.days >= 1 then
        ( delta.days, "日" )
    else if delta.hours >= 1 then
        ( delta.hours, "時間" )
    else if delta.minutes >= 1 then
        ( delta.minutes, "日" )
    else
        ( Basics.max delta.seconds 0, "秒" )


subscriptions : Model -> Sub Msg
subscriptions _ =
    every (20 * millisecond) Tick
