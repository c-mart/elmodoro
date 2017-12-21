{-
Todo:
- Break auto-ends when time expires
- Time display in browser's local time zone
- Stats for current day only?
- Save model to local storage
- Align stuff in right-hand column into table

Tada:
- Noise when finished
- Can work longer than countdown
- End break early
- Organize view functions based on application status
- Adjustable pomo length
- Pomodoro has start time, list of prev pomos shows start time of each
- All times in right-hand column show as "35m" or "2h 25m", not mm:ss
-}

module Elmodoro exposing (main)

import Html exposing (Html, div, h1, h2, h3, text, br, button, input, audio, table, tr, td, strong)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, type_, placeholder, value, src, autoplay)
import Result
import Task
import Time exposing (Time, second, minute)

main : Program Never Model Msg
main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick

init : (Model, Cmd Msg)
init =
  let
    defaultPomoLen = 3 * second
    defaultBreakLen = 5 * second
  in
    ( { timeRemain = defaultPomoLen
      , status = NoActivity
      , curPomo = { len = 0 * second, desc = "", startTime = Nothing }
      , prevPomos = []
      , pomoLen = defaultPomoLen
      , breakLen = defaultBreakLen
      }
    , Cmd.none
    )

type alias Model =
  { timeRemain : Time
  , status : Activity
  , curPomo : Pomodoro
  , prevPomos : List Pomodoro
  , pomoLen : Time
  , breakLen : Time
  }

type Activity
  = NoActivity
  | PomoOn
  | PomoPaused
  | Break

type alias Pomodoro
  = { len : Time
    , desc : String
    , startTime : Maybe Time
    }

type Msg
  = Tick Time
  | UpdateDesc String
  | UpdatePomoLen String
  | UpdatePomoStartTime Time
  | StartPomo
  | PausePomo
  | EndPomo
  | EndBreak

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateDesc newDesc -> (updateDesc model newDesc, Cmd.none)
    UpdatePomoLen newPomoLenStr -> (updatePomoLen model newPomoLenStr, Cmd.none)
    Tick _ -> tickUpdate model
    UpdatePomoStartTime time -> updatePomoStartTime model model.curPomo time
    StartPomo -> startPomo model
    PausePomo -> ( { model | status = PomoPaused }, Cmd.none )
    EndPomo -> ( endPomo model, Cmd.none )
    EndBreak -> ( { model | status = NoActivity, timeRemain = model.pomoLen }, Cmd.none )

updateDesc : Model -> String -> Model
updateDesc model newDesc =
  let
    oldPomo = model.curPomo
    newPomo = { oldPomo | desc = newDesc }
  in
    { model | curPomo = newPomo }

updatePomoLen : Model -> String -> Model
updatePomoLen model newPomoLenStr =
  let
    newPomoLen =
      newPomoLenStr
        |> String.toInt
        |> Result.withDefault 25
        |> toFloat
        |> (*) minute
  in
    { model | pomoLen = newPomoLen }

tickUpdate : Model -> (Model, Cmd Msg)
tickUpdate model =
  if List.member model.status [NoActivity, PomoPaused]
  then (model, Cmd.none)
  else
    if model.timeRemain > 0
    then ( { model |
             timeRemain = model.timeRemain - 1 * second
            {- This is technical debt -}
           , curPomo = incrementCurPomo model
           }
          , Cmd.none
          )
    else
      ( { model | curPomo = incrementCurPomo model }, Cmd.none )

updatePomoStartTime : Model -> Pomodoro -> Time -> (Model, Cmd Msg)
updatePomoStartTime model curPomo time =
  let
    newCurPomo = { curPomo | startTime = Just time }
  in
    ( { model | curPomo = newCurPomo }, Cmd.none )

{- This is technical debt -}
incrementCurPomo : Model -> Pomodoro
incrementCurPomo model =
  case model.status of
    PomoOn ->
      let
        pomo = model.curPomo
      in
        { pomo | len = pomo.len + 1 * second }
    _ -> model.curPomo

startPomo : Model -> (Model, Cmd Msg)
startPomo model =
  ( { model | status = PomoOn }, Task.perform UpdatePomoStartTime Time.now )

endPomo : Model -> Model
endPomo model =
  let
    newPrevPomos = model.curPomo :: model.prevPomos
  in
    { model | status = Break
    , timeRemain = model.breakLen
    , curPomo = { len = 0 * second, desc = "", startTime = Nothing }
    , prevPomos = newPrevPomos
    }

{- View helpers -}

pluralize : Int -> String -> String
pluralize number singularWord =
  case number of
    1 -> singularWord
    _ -> singularWord ++ "s"

timeDurationToClockString : Time -> String
timeDurationToClockString time =
  let
    minutes =
      time
        |> Time.inMinutes
        |> floor
        |> toString
        |> String.padLeft 2 '0'
    seconds =
      time
        |> Time.inSeconds
        |> floor
        |> \f -> f % 60
        |> toString
        |> String.padLeft 2 '0'
  in
    minutes ++ ":" ++ seconds

timeDurationToHMString : Time -> String
timeDurationToHMString time =
  let
    minutes =
      time
        |> Time.inMinutes
        |> floor
        |> toString
    seconds =
      time
        |> Time.inSeconds
        |> floor
        |> \f -> f % 60
        |> toString
  in
    minutes ++ "m " ++ seconds ++ "s"


clockTimeToString : Time -> String
clockTimeToString time =
  let
    hours =
      time
        |> Time.inHours
        |> floor
        |> \f -> f % 24
        |> toString
        |> String.padLeft 2 '0'
    minutes =
      time
        |> Time.inMinutes
        |> floor
        |> \f -> f % 60
        |> toString
        |> String.padLeft 2 '0'
  in
    hours ++ ":" ++ minutes

view : Model -> Html Msg
view model =
  let
    viewFunction =
      case model.status of
        NoActivity -> viewNoActivity
        PomoOn -> viewPomoOnOrPaused
        PomoPaused -> viewPomoOnOrPaused
        Break -> viewBreak
  in
  div [ class "container" ]
  [ h1 [] [ text "Elmodoro" ]
  , div [ class "row" ]
    [ div [ class "six columns" ]
      [ div []
        [ viewStatus model.status
        , br [] []
        , viewFunction model
        , viewControls model.status
        ]
      ]
    , div [ class "six columns" ]
      [ div []
        [ h3 [] [ text "Current pomodoro" ]
        , table [] [ pomoTableHeader, viewPomo model.curPomo ]
        ]
      , div []
        [ h3 [] [ text "Stats" ]
        , div [] [ viewStats model ]
        ]
      , div []
        [ h3 [] [ text "Previous pomodoros" ]
        , table []
            (List.append [ pomoTableHeader ] ( List.map viewPomo model.prevPomos ))

        ]
      ]
    ]
  , viewAudio model
  ]

pomoTableHeader : Html Msg
pomoTableHeader =
  tr []
    [ td [] [ strong [] [ text "Start time" ] ]
    , td [] [ strong [] [ text "Description" ] ]
    , td [] [ strong [] [ text "Duration" ] ]
    ]


viewStatus : Activity -> Html Msg
viewStatus status =
  case status of
    NoActivity ->
      text "Get ready to work on:"
    PomoOn ->
      text "You're working on:"
    PomoPaused ->
      text "Work session paused"
    Break ->
      text "Pomodoro complete, go take a break!"

viewNoActivity : Model -> Html Msg
viewNoActivity model =
  div []
  [ viewPomoDesc model.status model.curPomo.desc
  , viewTimeInput model.status model.pomoLen
  ]

viewPomoOnOrPaused : Model -> Html Msg
viewPomoOnOrPaused model =
  let
    timeRemainMessage =
      case model.timeRemain of
        0 -> "Time's up, but you can keep working"
        _ -> "Time remaining:"
  in
    div []
    [ viewPomoDesc model.status model.curPomo.desc
    , br [] []
    , text timeRemainMessage
    , br [] []
    , viewClock model.timeRemain
    ]

viewBreak : Model -> Html Msg
viewBreak model = div []
  [ viewClock model.timeRemain
  ]

viewPomoDesc : Activity -> String -> Html Msg
viewPomoDesc status desc =
 input [ type_ "text"
 , placeholder "What are you working on?"
 , value desc
 , onInput UpdateDesc
 ] []


viewTimeInput : Activity -> Time -> Html Msg
viewTimeInput status time =
  div []
  [ text "for "
  , input [ type_ "text"
          , value (time |> Time.inMinutes |> floor |> toString)
          , onInput UpdatePomoLen
          ] []
  , text (" " ++ pluralize (time |> Time.inMinutes |> floor) "minute")
  ]

viewClock : Time -> Html Msg
viewClock time =
  div [ class "digital-clock" ] [ text (timeDurationToClockString time) ]

viewControls : Activity -> Html Msg
viewControls status =
  case status of
    NoActivity ->
      div []
      [ button [ onClick StartPomo, class "button-primary" ] [ text "Start Working" ]
      ]
    PomoOn ->
      div []
      [ button [ onClick PausePomo, class "button-primary" ] [ text "Pause" ]
      , button [ onClick EndPomo ] [ text "End Pomodoro" ]
      ]
    PomoPaused ->
      div []
      [ button [ onClick StartPomo, class "button-primary" ] [ text "Resume" ]
      , button [ onClick EndPomo ] [ text "End Pomodoro" ]
      ]
    Break ->
      div []
      [ button [onClick EndBreak ] [ text "Finish Break" ]
      ]

viewPomo : Pomodoro -> Html Msg
viewPomo pomo =
  let
    desc = if String.isEmpty pomo.desc then "(no description)" else pomo.desc
    startTimeStr =
      case pomo.startTime of
        Just time -> (clockTimeToString time) ++ " "
        Nothing -> ""
  in
    tr []
    [ td [] [ text startTimeStr ]
    , td [] [ text desc ]
    , td [] [ text (timeDurationToHMString pomo.len) ]
    ]

viewStats : Model -> Html Msg
viewStats model =
  let
    pomos = model.curPomo :: model.prevPomos
    totalTime =
      List.foldl (\pomo totTime -> totTime + pomo.len) 0 pomos
    totalTimeStr = timeDurationToClockString totalTime
    completedPomos =
      pomos
        |> List.length
        |> \x -> x - 1
        |> toString
  in
    div [] [ text ("Total time worked: " ++ totalTimeStr)
           , br [] []
           , text ("Completed pomodoros: " ++ completedPomos)
           ]

viewAudio : Model -> Html Msg
viewAudio model =
  case model.timeRemain of
    0 ->
      audio [ src "bark.ogg", autoplay True ] []
    _ ->
      div [] []
