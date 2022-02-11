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

main = Browser.element {
    init = init,
    update = update,
    subscriptions = subscriptions,
    view = view
  }

type alias Model = {
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
    Reset
  | Tick
  | PlayPause
  | LightPress (Int, Int)

height = 8 
width = 8

blankLights = Matrix.repeat (height, width) Black

init : Flags -> (Model, Cmd Msg)
init flags = ({
  paused = True,
  clockTick = 0,
  lights = blankLights,
  onInit = [Constant Blue],
  onTick = [X,Y,Plus,ClockTick,Plus],
  onTouch = toggle Blue Cyan
  },
  Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.paused
  then Sub.none
  else Time.every 1000 (\_->Tick)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Reset ->
    ({model | lights = eval model.onInit model.lights 0, clockTick = 0},
     Cmd.none)
  Tick ->
    ({ model |
        lights = eval model.onTick model.lights model.clockTick,
        clockTick = model.clockTick + 1},
     Cmd.none)
  PlayPause -> ({model|paused = not model.paused}, Cmd.none)
  LightPress location ->
    let context = createContext model.lights model.clockTick location
        newPixel = evalCell model.onTouch [] context
    in ({ model | lights = Matrix.set location newPixel model.lights},
          Cmd.none)

----------
-- View --
----------

view model = Element.layout [] <|
  row [Element.width Element.fill, Element.height Element.fill] [
    programEntry model,
    column [Element.width (Element.fillPortion 5), Element.height Element.fill] [
      controls model,
      lightsView model.lights
      ]
    ]

programEntry : Model -> Element Msg
programEntry model = column [
    Element.height Element.fill,
    Element.width (Element.fillPortion 2)
    ]
  (List.map operationView model.onTick)

operationView : Operation -> Element Msg
operationView op = el [] (text (operationToString op))

operationToString op = case op of
  Constant c -> colorToString c
  Equal -> "Equal"
  This -> "This"
  If -> "If"
  Neighbors -> "Neighbors"
  Sum -> "Sum"
  X -> "X"
  Y -> "Y"
  ClockTick -> "ClockTick"
  Plus -> "Plus"
  

controls : Model -> Element Msg
controls model = row [] [
  button [] {onPress = Just Reset, label = text "Reset"},
  button [] {onPress = Just Tick, label = text "Step"},
  button [] {onPress = Just PlayPause,
             label = text (if model.paused then "Play" else "Pause")}
  ]

lightsView : Matrix Color -> Element Msg
lightsView lights =
  column [
    Element.width Element.fill,
    Element.height Element.fill
  ] (List.map2 lightRow (List.range 0 (height-1)) (Matrix.toList lights))

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
  ] { onPress = Just (LightPress (columnIndex, rowIndex)), label = text " " }

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
