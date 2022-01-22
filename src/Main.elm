module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Task
import Time
import Time.Extra


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { time : Maybe Time.Posix
    , zone : Maybe Time.Zone
    }


type RubbishDay
    = RubbishAndRecycling
    | RubbishAndGlass


init : () -> ( Model, Cmd Msg )
init () =
    ( { time = Nothing, zone = Nothing }
    , Task.perform identity (Task.map2 GotTime Time.here Time.now)
    )


type Msg
    = GotTime Time.Zone Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTime zone posix ->
            ( { model | zone = Just zone, time = Just posix }, Cmd.none )


view : Model -> Html Msg
view model =
    case ( model.zone, model.time ) of
        ( Just zone, Just time ) ->
            viewRubbishDay zone time

        _ ->
            Html.text ""


viewRubbishDay : Time.Zone -> Time.Posix -> Html msg
viewRubbishDay zone time =
    Html.div []
        [ case Time.toWeekday zone time of
            Time.Mon ->
                Html.text "Today is for "

            _ ->
                Html.text "The coming Monday is for "
        , case toNextRubbishDay zone time of
            RubbishAndRecycling ->
                Html.span [ Html.Attributes.style "background-color" "yellow" ]
                    [ Html.text "Rubbish and Recycling" ]

            RubbishAndGlass ->
                Html.span [ Html.Attributes.style "background-color" "cyan" ]
                    [ Html.text "Rubbish and Glass" ]
        ]


{-| Monday of a RubbishAndRecycling week.
-}
rubbishEpoch : Time.Zone -> Time.Posix
rubbishEpoch zone =
    Time.Extra.fromDateTuple zone ( 2021, Time.Nov, 1 )


millisInDay : Int
millisInDay =
    1000 * 60 * 60 * 24


toNextRubbishDay : Time.Zone -> Time.Posix -> RubbishDay
toNextRubbishDay zone time =
    let
        upcomingRubbishDay =
            Time.Extra.endOfWeek zone Time.Tue time

        millisSinceRubbishEpoch =
            Time.posixToMillis upcomingRubbishDay - Time.posixToMillis (rubbishEpoch zone)

        weeksSinceRubbishEpoch =
            millisSinceRubbishEpoch // millisInDay // 7
    in
    if (weeksSinceRubbishEpoch |> modBy 2) == 0 then
        RubbishAndRecycling

    else
        RubbishAndGlass
