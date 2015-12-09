import String
import Task
import Http
import Html
import Array
import Regex
import Matrix


-- main/view


main : Signal Html.Html
main =
  Signal.map view inputMailbox.signal


view : String -> Html.Html
view input =
  -- Matrix.Extra.prettyPrint <| day6part1 input
  Html.div []
    [ --Html.div [] [ Html.text ("Day 6 Part 1: " ++ (toString <| day6part1 input)) ]
    {--,--} Html.div [] [ Html.text ("Day 6 Part 2: " ++ (toString <| day6part2 input)) ]
    ]


inputMailbox : Signal.Mailbox String
inputMailbox =
  Signal.mailbox ""


port requests : Task.Task Http.Error (Result.Result Http.Error ())
port requests =
  Http.getString "input.txt"
  `Task.andThen`
  Signal.send inputMailbox.address
  |> Task.toResult


-- Part 1
zip : List a -> List a -> List (a, a)
zip = List.map2 (,)


type alias Lights a = Matrix.Matrix a
type alias Coordinates = ((Int, Int), (Int, Int))
type alias Operation a =
  { operation: (a -> a)
  , coordinates : List (Int, Int)
  }


getRangeFromList : List Int -> Coordinates
getRangeFromList list =
  case list of
    [a, b, c, d] ->
      ((a, b), (c, d))
    _ ->
      ((-1, -1), (-1, -1)) -- noop


getOperationP1 : String -> Operation Bool
getOperationP1 string =
  let
    coordinates = getCoordsInRange <| getRangeFromList <| List.map (\match -> (Result.withDefault 0 << String.toInt) match.match ) <| Regex.find Regex.All (Regex.regex "(\\d+)") (Debug.log "" <| string)
  in
    case String.slice 6 7 string of
      "n" ->
        { operation = (\_ -> True), coordinates = coordinates }
      "f" ->
        { operation = (\_ -> False), coordinates = coordinates }
      _ ->
        { operation = not, coordinates = coordinates }


getCoordsInRange : Coordinates -> List (Int, Int)
getCoordsInRange ((x1, y1), (x2, y2)) =
  let
    xrange = [x1..x2]
    yrange = [y1..y2]
  in
    List.foldl (\x acc -> acc ++ (zip (List.repeat (List.length yrange) x) yrange)) [] xrange


applyOperation : Operation a -> Lights a -> Lights a
applyOperation operation lights =
  List.foldl (\coordinate lights -> Matrix.update (fst coordinate) (snd coordinate) operation.operation lights) lights operation.coordinates


day6part1 : String -> Int
day6part1 input =
  let
    lights = Matrix.repeat 1000 1000 False
  in
    Array.length <| Matrix.filter identity <| List.foldl (getOperationP1 >> applyOperation) lights (List.filter (\x -> not <| String.isEmpty x) <| String.split "\n" input)


-- Part 2
getOperationP2 : String -> Operation Int
getOperationP2 string =
  let
    coordinates = getCoordsInRange <| getRangeFromList <| List.map (\match -> (Result.withDefault 0 << String.toInt) match.match ) <| Regex.find Regex.All (Regex.regex "(\\d+)") (Debug.log "" <| string)
  in
    case String.slice 6 7 string of
      "n" ->
        { operation = (+) 1, coordinates = coordinates }
      "f" ->
        { operation = (\value -> if value == 0 then 0 else value - 1), coordinates = coordinates }
      _ ->
        { operation = (+) 2, coordinates = coordinates }


day6part2 : String -> Int
day6part2 input =
  let
    lights = Matrix.repeat 1000 1000 0
  in
    List.sum << Array.toList << .data <| List.foldl (getOperationP2 >> applyOperation) lights (List.filter (\x -> not <| String.isEmpty x) <| String.split "\n" input)
