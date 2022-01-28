module Main exposing (..)

import Browser exposing (element)
import Html
import Element exposing (Element, el, text, row, column)
import Element.Background as Background
import Element.Border as Border
import Element.Input exposing (button)
import Matrix exposing (Matrix)
import LightStack
import Time

main = Browser.element {
    init = init,
    update = update,
    subscriptions = subscriptions,
    view = view
  }

type alias Model = {
  paused : Bool,
  lights : Matrix LightStack.Color,
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

blankLights = Matrix.repeat (height, width) (LightStack.Color 0 0 0)

init : Flags -> (Model, Cmd Msg)
init flags = ({
  paused = True,
  lights = blankLights,
  onInit = [LightStack.Constant LightStack.blue],
  onTick = LightStack.gol,
  onTouch = LightStack.toggle LightStack.blue LightStack.cyan
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
    ({model | lights = LightStack.eval model.onInit model.lights}, Cmd.none)
  Tick ->
    ({model | lights = LightStack.eval model.onTick model.lights}, Cmd.none)
  PlayPause -> ({model|paused = not model.paused}, Cmd.none)
  LightPress location ->
    case Matrix.get location model.lights of
      Just pixel ->
        let newPixel = LightStack.evalCellWithNeighbors model.onTouch model.lights location pixel
        in ({ model | lights = Matrix.set location newPixel model.lights},
            Cmd.none)
      Nothing -> (model, Cmd.none)

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

operationView : LightStack.Operation -> Element Msg
operationView op = case op of
  LightStack.Constant v -> case v of
    LightStack.VNum n -> el [] <| text ("Constant" ++ String.fromInt n)
    LightStack.VColor c -> el [] (text "Constant")
    LightStack.VList l -> el [] (text "List")
  LightStack.Equal -> el [] (text "Equal")
  LightStack.This -> el [] (text "This")
  LightStack.If -> el [] (text "If")
  LightStack.Neighbors -> el [] (text "Neighbors")
  LightStack.Sum -> el [] (text "Sum")

controls : Model -> Element Msg
controls model = row [] [
  button [] {onPress = Just Reset, label = text "Reset"},
  button [] {onPress = Just Tick, label = text "Step"},
  button [] {onPress = Just PlayPause,
             label = text (if model.paused then "Play" else "Pause")}
  ]

lightsView : Matrix LightStack.Color -> Element Msg
lightsView lights =
  column [
    Element.width Element.fill,
    Element.height Element.fill
  ] (List.map2 lightRow (List.range 0 (height-1)) (Matrix.toList lights))

lightRow : Int -> List LightStack.Color -> Element Msg
lightRow rowIndex lights = row [
  Element.width Element.fill,
  Element.height Element.fill
  ] (List.map2 (lightCell rowIndex) (List.range 0 (width-1)) lights)

lightCell : Int -> Int -> LightStack.Color -> Element Msg
lightCell rowIndex columnIndex color = button [
  Background.color (lsColorToColor color),
  Border.color (Element.rgb 0.66 0.66 0.66),
  Border.width 5,
  Element.width (Element.fillPortion 1),
  Element.height (Element.fillPortion 1)
  ] { onPress = Just (LightPress (columnIndex, rowIndex)), label = text " " }

lsColorToColor : LightStack.Color -> Element.Color
lsColorToColor c =
  let maxColor = 1
  in Element.rgb
       (toFloat c.red/maxColor)
       (toFloat c.green/maxColor)
       (toFloat c.blue/maxColor)
