module LightStack exposing (..)

import Matrix exposing (Matrix)
import List.Extra

type alias Program  = List Operation

type Operation =
    Constant Color
  | Equal
  | This
  | If
  | Neighbors
  | Sum

type Value =
    VColor Color
  | VList (List Value)
 
type alias Context = {
  this : Color,
  location : (Int, Int),
  neighbors : List Color
  }

type Color = Black | Blue | Green | Cyan | Red | Magenta | Yellow | White

errorValue = Magenta

toggle a b = [
  Constant a,
  Constant b,
  Constant a,
  This,
  Equal,
  If
  ]

gol = [
  Constant Blue,
  Constant Cyan,
  Neighbors, Constant Cyan, Equal, Sum,
  Constant Cyan,
  Equal,
  If,
  This,
  Neighbors, Constant Cyan, Equal, Sum,
  Constant Green,
  Equal,
  If
  ]

eval : Program -> Matrix Color -> Matrix Color
eval program grid =
  Matrix.map
    (evalCell program [])
    (Matrix.indexedMap (\l _ -> createContext grid l) grid)

createContext : Matrix Color -> (Int, Int) -> Context
createContext m loc = {
  this = Maybe.withDefault errorValue (Matrix.get loc m),
  location = loc,
  neighbors = getNeighbors m loc
  }

getNeighbors : Matrix Color -> (Int, Int) -> List Color
getNeighbors m loc =
  let (width, height) = Matrix.size m
      inc limit n = if n+1 == limit then 0 else n+1
      dec limit n = if n == 0 then limit-1 else n-1
      north (x, y) = (x, dec height y)
      south (x, y) = (x, inc height y)
      east (x, y) =  (inc width x, y)
      west (x, y) = (dec width x, y)
  in [
    Maybe.withDefault errorValue <| Matrix.get (north (west loc)) m,
    Maybe.withDefault errorValue <| Matrix.get (north loc) m,
    Maybe.withDefault errorValue <| Matrix.get (north (east loc)) m,
    Maybe.withDefault errorValue <| Matrix.get (west loc) m,
    Maybe.withDefault errorValue <| Matrix.get (east loc) m,
    Maybe.withDefault errorValue <| Matrix.get (south (west loc)) m,
    Maybe.withDefault errorValue <| Matrix.get (south loc) m,
    Maybe.withDefault errorValue <| Matrix.get (south (east loc)) m
    ]

evalCell : Program -> List Value -> Context -> Color
evalCell program stack context = case program of
  [] -> case stack of
          (VColor c::_) -> c
          _ -> errorValue
  (op::prog) ->
    evalCell prog (evalStep op context stack) context

evalStep : Operation -> Context -> List Value -> List Value
evalStep op context stack = case op of
  Constant c -> VColor c::stack
  Equal -> applyBinOp (\a b->if a==b then VColor Blue else VColor Black) stack
  This -> VColor context.this::stack
  If ->
    case stack of
      (cond::conseq::alt::restStack) ->
        (if cond /= VColor Black
         then conseq
         else alt)::restStack
      _ -> [VColor errorValue]
  Neighbors ->
    let neighborsValue = VList (List.map VColor context.neighbors)
    in neighborsValue::stack
  Sum ->
    case stack of
      (VList l :: restOfStack) ->
        VColor (intToColor (List.foldl (+) 0 (List.map getNum l)))::restOfStack
      _ -> [VColor errorValue]

applyBinOp : (Value -> Value -> Value) -> List Value -> List Value
applyBinOp f stack = case stack of
  (VList a :: VList b :: restOfStack) ->
    VList (List.map2 f a b) :: restOfStack
  (VList a :: b :: restOfStack) ->
    VList (List.map (flip f b) a) :: restOfStack
  (a :: VList b :: restOfStack) ->
    VList (List.map (f a) b) :: restOfStack
  (a :: b :: restOfStack) ->
    f a b :: restOfStack
  _ -> stack

getNum : Value -> Int
getNum v = case v of
  VColor a -> colorToInt a
  VList a -> 0 -- FIXME

colorToInt : Color -> Int
colorToInt color = case color of
  Black -> 0
  Blue -> 1
  Green -> 2
  Cyan -> 3
  Red -> 4
  Magenta -> 5
  Yellow -> 6
  White -> 7

intToColor : Int -> Color
intToColor n = Maybe.withDefault errorValue (
  List.Extra.getAt
    (modBy 8 n)
    [Black, Blue, Green, Cyan, Red, Magenta, Yellow, White])

flip f = (\a b->f b a)
