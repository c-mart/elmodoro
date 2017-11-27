{-
Todo:
- Noise when finished
- Adjustable pomo length
- Can work longer than countdown
- Pomodoro has start time, list of prev pomos shows start day/time of each
- Stats for current day only?
- All times in right-hand column show as "35m" or "2h 25m", not mm:ss
- Save model to local storage
-}

module Elmodoro exposing (main)

import Html exposing (Html, div, h1, h2, h3, text, br, button, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, type_, placeholder, value)
import Time exposing (Time, second, minute)

main : Program Never Model Msg
main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

type alias Model =
  { timeRemain : Time
  , status : Activity
  , curPomo : Pomodoro
  , prevPomos : List Pomodoro
  , pomoLength : Time
  , breakLength : Time
  }

init : (Model, Cmd Msg)
init =
  let defaultPomoLength = 5 * second
  in
    ( { timeRemain = defaultPomoLength
      , status = NoActivity
      , curPomo = { length = 0 * second, desc = "" }
      , prevPomos = []
      , pomoLength = defaultPomoLength
      , breakLength = 5 * second
      }
    , Cmd.none
    )

type Msg
  = Tick Time
  | UpdateDesc String
  | StartPomo
  | PausePomo
  | EndActivity

type Activity
  = NoActivity
  | PomoOn
  | PomoPaused
  | Break

type alias Pomodoro
  = { length : Time
    , desc : String
    }

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateDesc newDesc -> (updateDesc model newDesc, Cmd.none)
    Tick _ -> tickUpdate model
    StartPomo -> ( { model | status = PomoOn }, Cmd.none )
    PausePomo -> ( { model | status = PomoPaused }, Cmd.none )
    EndActivity -> ( { model | status = NoActivity
                             , timeRemain = model.pomoLength
                     }
                   , Cmd.none
                   )

updateDesc : Model -> String -> Model
updateDesc model newDesc =
  let
    oldPomo = model.curPomo
    newPomo = { oldPomo | desc = newDesc }
  in
    { model | curPomo = newPomo }

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
      if model.status == PomoOn
      then ( finishPomo model, Cmd.none)
      else ( { model | status = NoActivity, timeRemain = model.pomoLength }, Cmd.none )

{- This is technical debt -}
incrementCurPomo : Model -> Pomodoro
incrementCurPomo model =
  case model.status of
    PomoOn ->
      let
        pomo = model.curPomo
      in
        { pomo | length = pomo.length + 1 * second }
    _ -> model.curPomo


finishPomo : Model -> Model
finishPomo model =
  let
    newPrevPomos = model.curPomo :: model.prevPomos
  in
    { model | status = Break
    , timeRemain = model.breakLength
    , curPomo = { length = 0 * second, desc = "" }
    , prevPomos = newPrevPomos
    }

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick

view : Model -> Html Msg
view model =
  div [ class "container" ]
  [ h1 [] [ text "Elmodoro" ]
  , div [ class "row" ]
    [ div [ class "six columns" ]
      [ div []
        [ viewStatus model.status
        , br [] []
        , input [ type_ "text"
                , placeholder "What are you working on?"
                , value model.curPomo.desc
                , onInput UpdateDesc
                ] []
        ]
      , div []
        [ div [ class "digital-clock" ] [ text (timeToString model.timeRemain) ]
        ]
      , div []
        [ viewControls model.status
        ]
      ]
    , div [ class "six columns" ]
      [ div []
        [ h3 [] [ text "Current pomodoro" ]
        , viewPomo model.curPomo
        ]
      , div []
        [ h3 [] [ text "Stats" ]
        , div [] [ viewStats model ]
        ]
      , div []
        [ h3 [] [ text "Previous pomodoros" ]
        , div [] (List.map viewPomo model.prevPomos)
        ]
      ]
    ]
  ]

timeToString : Time -> String
timeToString time =
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
      ]
    PomoPaused ->
      div []
      [ button [ onClick StartPomo, class "button-primary" ] [ text "Resume" ]
      , button [ onClick EndActivity ] [ text "End Pomodoro" ]
      ]
    Break ->
      div []
      [ button [onClick EndActivity ] [ text "Finish Break" ]
      ]

viewPomo : Pomodoro -> Html Msg
viewPomo pomo =
  let
    desc = if String.isEmpty pomo.desc then "(no description)" else pomo.desc
  in
    div [] [ text (desc ++ " for " ++ timeToString pomo.length) ]

viewStats : Model -> Html Msg
viewStats model =
  let
    pomos = model.curPomo :: model.prevPomos
    totalTime =
      List.foldl (\pomo totTime -> totTime + pomo.length) 0 pomos
    totalTimeStr = timeToString totalTime
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
