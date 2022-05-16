module LightStack exposing (..)

import Matrix exposing (Matrix)
import List.Extra
import Random

type alias Program  = List Operation

type alias Stack = List Color

type Operation =
    Constant Color
  | Equal
  | This
  | If
  | X
  | Y
  | ClockTick
  | Plus
  | Times
  | Count
  | Get
  | Random

type alias Context = {
  this : Color,
  location : (Int, Int),
  neighbors : List Color,
  clockTick : Int
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
  Constant Red,
  Constant Magenta,
  Constant Magenta,
  Count,
  Constant Cyan,
  Equal,
  If,
  This,
  Constant Magenta,
  Count,
  Constant Green,
  Equal,
  If
  ]

shootingStars = [
  Constant Yellow, Constant Blue, Constant White, Random, If,
  Constant Blue, Constant Blue, Constant Yellow, Constant Red,
  Random, If, Constant Green, Get, Constant Yellow, Equal, If,
  Constant White, Random, If
  ]

eval : Program -> Matrix Color -> Int -> Random.Seed -> Matrix Color
eval program grid clockTick seed =
  matrixMap2
    (evalCell program [])
    (Matrix.indexedMap (\l _ -> createContext grid clockTick l) grid)
    (generateRandomSeeds seed)

createContext : Matrix Color -> Int -> (Int, Int) -> Context
createContext m clockTick loc = {
  this = Maybe.withDefault errorValue (Matrix.get loc m),
  location = loc,
  neighbors = getNeighbors m loc,
  clockTick = clockTick
  }

getNeighbors : Matrix Color -> (Int, Int) -> List Color
getNeighbors m loc =
  let (width, height) = Matrix.size m
      inc limit n = if n+1 == limit then 0 else n+1
      dec limit n = if n == 0 then limit-1 else n-1
      north (x, y) = (x, dec height y)
      south (x, y) = (x, inc height y)
      east (x, y) = (inc width x, y)
      west (x, y) = (dec width x, y)
  in [
    Maybe.withDefault errorValue <| Matrix.get (north (west loc)) m,
    Maybe.withDefault errorValue <| Matrix.get (north loc) m,
    Maybe.withDefault errorValue <| Matrix.get (north (east loc)) m,
    Maybe.withDefault errorValue <| Matrix.get (east loc) m,
    Maybe.withDefault errorValue <| Matrix.get (south (east loc)) m,
    Maybe.withDefault errorValue <| Matrix.get (south loc) m,
    Maybe.withDefault errorValue <| Matrix.get (south (west loc)) m,
    Maybe.withDefault errorValue <| Matrix.get (west loc) m
    ]

generateRandomSeeds : Random.Seed -> Matrix Random.Seed
generateRandomSeeds seed =
  let (seeds, _) = Random.step (Random.list 64 Random.independentSeed) seed
  in Matrix.fromList (unflatted 8 seeds)

unflatted : Int -> List a -> List (List a)
unflatted width l = case l of
  [] -> []
  _ -> (List.take width l) :: unflatted width (List.drop width l)

evalCell : Program -> Stack -> Context -> Random.Seed -> Color
evalCell program stack context seed = case program of
  [] ->
    case stack of
      (c::_) -> c
      _ -> errorValue
  (op::prog) ->
    let (nextSeed, continueSeed) = Random.step Random.independentSeed seed
        newStack = evalStep op context nextSeed stack
    in evalCell prog newStack context continueSeed

evalStep : Operation -> Context -> Random.Seed -> Stack -> Stack
evalStep op context seed stack = case op of
  Constant c -> c::stack
  Equal -> applyBinOp stack (\a b->(if a==b then Blue else Black))
  This -> context.this::stack
  If ->
    case stack of
      (cond::conseq::alt::restOfStack) ->
        (if cond /= Black
         then conseq
         else alt)::restOfStack
      _ -> [errorValue]
  X -> intToColor (Tuple.first context.location) :: stack
  Y -> intToColor (Tuple.second context.location) :: stack
  ClockTick -> intToColor (modBy 8 context.clockTick) :: stack
  Plus ->
    applyBinOp stack (\a b->
      intToColor (modBy 8 (colorToInt a + colorToInt b)))
  Times ->
    applyBinOp stack (\a b->
      intToColor (modBy 8 (colorToInt a * colorToInt b)))
  Count ->
    applyUnaryOp stack (\c->
      intToColor
        (List.length (List.filter (\i -> i == c) context.neighbors)))
  Get ->
    applyUnaryOp stack (\c->
      Maybe.withDefault
        errorValue
        (List.Extra.getAt (colorToInt c) context.neighbors))
  Random ->
    applyUnaryOp stack (\c->
      let (rando, _) = Random.step (Random.int 0 (colorToInt c)) seed
      in intToColor rando)

applyBinOp : Stack -> (Color -> Color -> Color) -> Stack
applyBinOp stack f = case stack of
  (a :: b :: restOfStack) ->
    (f a b) :: restOfStack
  _ -> stack

applyUnaryOp : Stack -> (Color -> Color) -> Stack
applyUnaryOp stack f = case stack of
  (c :: restOfStack) -> f c :: restOfStack
  _ -> stack

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

matrixMap2 : (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
matrixMap2 f m1 m2 =
  let l1 = Matrix.toList m1
      l2 = Matrix.toList m2
  in Matrix.fromList <| 
     List.map2 (\subl1 subl2->List.map2 f subl1 subl2) l1 l2
