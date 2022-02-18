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
  onInit : LightStack.Program,
  onTick : LightStack.Program,
  onTouch : LightStack.Program
  }

type alias Flags = {
  }

type Msg =
    NewSeed Random.Seed
  | Reset
  | Tick
  | PlayPause
  | LightPress (Int, Int)

height = 8 
width = 8

init : Flags -> (Model, Cmd Msg)
init flags = ({
  seed = Random.initialSeed 0,
  paused = True,
  clockTick = 0,
  lights = Matrix.repeat (height, width) Black,
  onInit = [Constant Blue],
  onTick = [Constant Blue, Random],
  onTouch = [Constant Yellow]
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
         lights = eval model.onInit model.lights 0 model.seed,
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

----------
-- View --
----------

view model = Element.layout [] <|
  column [Element.width Element.fill, Element.height Element.fill] [
    outputView model,
    inputView model
  ]

-- Element.width Element.fill
-- Element.width (Element.fillPortion 5)

outputView : Model -> Element Msg
outputView model = 
  column [Element.width (Element.fillPortion 2),
          Element.height (Element.fillPortion 1)] [
    controlsView model,
    lightsView model.lights
    ]

inputView : Model -> Element Msg
inputView model = column [
    Element.width (Element.fillPortion 2),
    Element.height (Element.fillPortion 1)
    ] [
  blocksView model,
  row [
    Element.width Element.fill,
    Element.height Element.fill
  ] [
    programView model.onInit,
    programView model.onTick,
    programView model.onTouch
    ]
  ]

blocksView : Model -> Element Msg
blocksView model = el [] (text "blocks")

programView : LightStack.Program -> Element Msg
programView program = column [
    Border.width 1,
    Element.width Element.fill,
    Element.height Element.fill,
    Element.alignBottom,
    Element.padding 30,
    Element.spacing 20
  ]
  (List.map operationView program)

operationView : Operation -> Element Msg
operationView op = el [
  Element.width Element.fill,
  Background.color (Element.rgb 127 127 127),
  Border.rounded 14,
  Border.dashed,
  Border.width 4,
  Element.padding 3,
  Element.alignBottom,
  Element.centerX
  ] (el [Element.centerX] (text (operationToString op)))

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
controlsView model = row [] [
  button [] {onPress = Just Reset, label = text "Reset"},
  button [] {onPress = Just Tick, label = text "Step"},
  button [] {onPress = Just PlayPause,
             label = text (if model.paused then "Play" else "Pause")}
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
