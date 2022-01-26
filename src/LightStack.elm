{-
TODO:
* errors are handled with evaluating to black, reconsider this
-}

module LightStack exposing (..)

import Matrix exposing (Matrix)

type alias Program  = List Operation

type Operation =
    Constant Value
  | Equal
  | This
  | If
  | Neighbors
  | Sum

type Value =
    VColor Color
  | VNum Int
  | VList (List Value)
 
type alias Color = {
  red : Int,
  green : Int,
  blue : Int
  }

red = VColor (Color 1 0 0)
green = VColor (Color 0 1 0)
blue = VColor (Color 0 0 1)
cyan = VColor (Color 0 1 1)
magenta = VColor (Color 1 0 1)
yellow = VColor (Color 1 1 0)
black = VColor (Color 0 0 0)
white = VColor (Color 1 1 1)
errorValue = Color 1 0 1

toggle a b = [
  Constant a,
  Constant b,
  Constant a,
  This,
  Equal,
  If
  ]

gol = [
  Constant blue,
  Constant cyan,
  Neighbors, Constant cyan, Equal, Sum,
  Constant (VNum 3),
  Equal,
  If,
  This,
  Neighbors, Constant cyan, Equal, Sum,
  Constant (VNum 2),
  Equal,
  If
  ]

eval : Program -> Matrix Color -> Matrix Color
eval program grid =
  Matrix.indexedMap (evalCellWithNeighbors program grid) grid

evalCellWithNeighbors : Program -> Matrix Color -> (Int, Int) -> Color -> Color
evalCellWithNeighbors program m loc c =
  evalCell program c (getNeighbors m loc) []

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
    Maybe.withDefault (Color 0 0 0) <| Matrix.get (north (west loc)) m,
    Maybe.withDefault (Color 0 0 0) <| Matrix.get (north loc) m,
    Maybe.withDefault (Color 0 0 0) <| Matrix.get (north (east loc)) m,
    Maybe.withDefault (Color 0 0 0) <| Matrix.get (west loc) m,
    Maybe.withDefault (Color 0 0 0) <| Matrix.get (east loc) m,
    Maybe.withDefault (Color 0 0 0) <| Matrix.get (south (west loc)) m,
    Maybe.withDefault (Color 0 0 0) <| Matrix.get (south loc) m,
    Maybe.withDefault (Color 0 0 0) <| Matrix.get (south (east loc)) m
    ]

evalCell : Program -> Color -> List Color -> List Value -> Color
evalCell program color neighbors stack = case program of
  [] -> case stack of
          (VColor c::_) -> c
          _ -> errorValue
  (op::prog) ->
    evalCell prog color neighbors (evalStep op color neighbors stack)

evalStep : Operation -> Color -> List Color -> List Value -> List Value
evalStep op color neighbors stack = case op of
  Constant c -> c::stack
  Equal -> applyBinOp (\a b->if a==b then VNum 1 else VNum 0) stack
  This -> VColor color::stack
  If ->
    case stack of
      (cond::conseq::alt::restStack) ->
        (if cond == VNum 1
         then conseq
         else alt)::restStack
      _ -> [VColor errorValue]
  Neighbors ->
    let neighborsValue = VList (List.map VColor neighbors)
    in neighborsValue::stack
  Sum ->
    case stack of
      (VList l :: restOfStack) -> 
        VNum (List.foldl (+) 0 (List.map getNum l))::restOfStack
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
  VNum n -> n
  VColor a -> 0 -- FIXME
  VList a -> 0 -- FIXME

flip f = (\a b->f b a)
