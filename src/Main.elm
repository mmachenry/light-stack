module Main exposing (..)

import Browser exposing (element)
import Html
import Element exposing (Element, el, text, row, column)
import Element.Background as Background
import Element.Border as Border
import Element.Input exposing (button)
import Matrix exposing (Matrix)
import LightStack exposing (..)
import Time
import Random
import List.Extra

main = Browser.element {
  init = init,
  update = update,
  subscriptions = subscriptions,
  view = view
  }

type alias Model = {
  seed : Random.Seed,
  paused : Bool,
  clockTick : Int,
  lights : Matrix Color,
  onReset : LightStack.Program,
  onTick : LightStack.Program,
  onTouch : LightStack.Program,
  activeProgram : Event
  }

type alias Flags = {
  }

type Msg =
    NewSeed Random.Seed
  | Reset
  | Tick
  | PlayPause
  | LightPress (Int, Int)
  | AddOp Operation
  | RemoveOp Event Int
  | SetActiveProgram Event

type Event = OnReset | OnTick | OnTouch

height = 8 
width = 8

init : Flags -> (Model, Cmd Msg)
init flags = ({
  seed = Random.initialSeed 0,
  paused = True,
  clockTick = 0,
  lights = Matrix.repeat (height, width) Black,
  onReset = [Constant Red, Constant Magenta, Constant Blue, Random, If],
  onTick = gol,
  onTouch = toggle Red Magenta,
  activeProgram = OnTick
  },
  Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.paused
  then Sub.none
  else Time.every 250 (\_->Tick)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  NewSeed seed -> ({ model | seed = seed}, Cmd.none)
  Reset ->
    ({ model |
         lights = eval model.onReset model.lights 0 model.seed,
         clockTick = 0},
     Random.generate NewSeed Random.independentSeed)
  Tick ->
    ({ model |
        lights = eval model.onTick model.lights model.clockTick model.seed,
        clockTick = model.clockTick + 1},
     Random.generate NewSeed Random.independentSeed)
  PlayPause -> ({model|paused = not model.paused}, Cmd.none)
  LightPress location ->
    let context = createContext model.lights model.clockTick location
        newPixel = evalCell model.onTouch [] context model.seed
    in ({ model | lights = Matrix.set location newPixel model.lights},
          Cmd.none)
  AddOp op ->
    programUpdater model (\p->p++[op])
  RemoveOp event i ->
    programUpdater
      { model | activeProgram = event }
      (\p->List.Extra.removeAt i p)
  SetActiveProgram event ->
    ({ model | activeProgram = event }, Cmd.none)

programUpdater : Model -> (LightStack.Program -> LightStack.Program) -> (Model, Cmd Msg)
programUpdater model f = case model.activeProgram of
  OnReset -> ({ model | onReset = f model.onReset }, Cmd.none)
  OnTick -> ({ model | onTick = f model.onTick }, Cmd.none)
  OnTouch -> ({ model | onTouch = f model.onTouch }, Cmd.none)

----------
-- View --
----------

view model = Element.layout [] <|
  row [
    Element.padding 20,
    Element.spacing 20,
    Element.width Element.fill,
    Element.height Element.fill
  ] [
    column [
      Element.width (Element.fillPortion 5),
      Element.height Element.fill
    ] [
      controlsView model,
      row [
        Element.width Element.fill,
        Element.height (Element.fillPortion 8)
      ] [
        lightsView model.lights,
        blocksView model
      ],
      row [
        Element.padding 20,
        Element.spacing 20,
        Element.width Element.fill,
        Element.height (Element.fillPortion 5)
      ] [
        programView model OnReset,
        programView model OnTouch
      ]
    ],
    column [
      Element.width (Element.fillPortion 2),
      Element.height Element.fill
    ] [
      programView model OnTick
    ]
  ]

blocksView : Model -> Element Msg
blocksView model =
  let row1 = List.map Constant [Black, Blue, Green, Cyan]
      row2 = List.map Constant [Red, Magenta, Yellow, White]
      row3 = [ Equal, This, If, X, Y, ClockTick ]
      row4 = [ Plus, Count, Get, Random ]
      opview o = myBtn (operationToString o) (AddOp o)
  in column [Element.centerX, Element.height (Element.fillPortion 2)] <|
       List.map (\opRow->
         row [ Element.spacing 10, Element.padding 10]
          (List.map opview opRow))
         [row1,row2,row3,row4]

programView : Model -> Event -> Element Msg
programView model event =
  let opview o i = myBtn (operationToString o) (RemoveOp event i)
      program = case event of
        OnReset -> model.onReset
        OnTick -> model.onTick
        OnTouch -> model.onTouch
      label = case event of
        OnReset -> "on Reset"
        OnTick -> "on Tick"
        OnTouch -> "on Touch"
  in column [
      Element.width Element.fill,
      Element.height Element.fill,
      Border.width 10,
      Border.color (if event == model.activeProgram
                    then Element.rgb 0.7 0 0.7
                    else Element.rgb 0 0 0)
    ] [
    myBtn label (SetActiveProgram event),
    column [
      Element.width Element.fill,
      Element.height Element.fill,
      Element.alignBottom,
      Element.padding 30,
      Element.spacing 20
      ]
      (List.reverse
        (List.map2 opview
          program
          (List.range 0 (List.length program - 1))))
    ]

myBtn : String -> Msg -> Element Msg
myBtn opName msg = button [
  Element.width Element.fill,
  Background.color (Element.rgb 127 127 127),
  Border.rounded 14,
  Border.dashed,
  Border.width 4,
  Element.padding 5,
  Element.alignBottom,
  Element.centerX
  ] { onPress = Just msg, label = el [Element.centerX] (text opName) }

operationToString op = case op of
  Constant c -> colorToString c
  Equal -> "Equal"
  This -> "This"
  If -> "If"
  X -> "X"
  Y -> "Y"
  ClockTick -> "ClockTick"
  Plus -> "Plus"
  Count -> "Count"
  Get -> "Get"
  Random -> "Random"


lightsView : Matrix Color -> Element Msg
lightsView lights =
  column [
    Element.width Element.fill,
    Element.height Element.fill
  ] (List.map2 lightRow (List.range 0 (height-1)) (Matrix.toList lights))

controlsView : Model -> Element Msg
controlsView model = row [Element.padding 10, Element.spacing 10, Element.height (Element.fillPortion 1)] [
  myBtn "Reset" Reset,
  myBtn "Step" Tick,
  myBtn (if model.paused then "Play" else "Pause") PlayPause
  ]


lightRow : Int -> List Color -> Element Msg
lightRow rowIndex lights = row [
  Element.width Element.fill,
  Element.height Element.fill
  ] (List.map2 (lightCell rowIndex) (List.range 0 (width-1)) lights)

lightCell : Int -> Int -> Color -> Element Msg
lightCell rowIndex columnIndex color = button [
  Background.color (lsColorToColor color),
  Border.color (Element.rgb 0.66 0.66 0.66),
  Border.width 5,
  Element.width (Element.fillPortion 1),
  Element.height (Element.fillPortion 1)
  ] {
    onPress = Just (LightPress (columnIndex, rowIndex)),
    label = text " "
  }

lsColorToColor : Color -> Element.Color
lsColorToColor color = case color of
  Black -> Element.rgb 0 0 0
  Blue -> Element.rgb 0 0 255
  Green -> Element.rgb 0 255 0
  Cyan -> Element.rgb 0 255 255
  Red -> Element.rgb 255 0 0
  Magenta -> Element.rgb 255 0 255
  Yellow -> Element.rgb 255 255 0
  White -> Element.rgb 255 255 255

colorToString : Color -> String
colorToString color = case color of
  Black -> "Black"
  Blue -> "Blue"
  Green -> "Green"
  Cyan -> "Cyan"
  Red -> "Red"
  Magenta -> "Magenta"
  Yellow -> "Yellow"
  White -> "White"
