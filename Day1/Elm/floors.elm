import String
import Task
import Http
import Html


-- main/view


main : Signal Html.Html
main =
  Signal.map view inputMailbox.signal


view : String -> Html.Html
view input =
  Html.div []
    [ Html.div [] [ Html.text ("Day 1 Part 1: " ++ (toString <| day1part1 input)) ]
    , Html.div [] [ Html.text ("Day 1 Part 2: " ++ (toString <| day1part2 input)) ]
    ]


inputMailbox : Signal.Mailbox String
inputMailbox =
  Signal.mailbox ""


port requests : Task.Task Http.Error (Result Http.Error ())
port requests =
  Http.getString "input.txt"
  `Task.andThen`
  Signal.send inputMailbox.address
  |> Task.toResult


-- Part 1
type alias Floor = Int


floor : Int
floor = 0


-- Converts the initial input into a List of values to add to the initial value
parensToValues : String -> List Int
parensToValues parens =
  String.split "" parens
  |> List.map traverseFloor


-- Used by parensToValues to decide what paren means what
traverseFloor : String -> Int
traverseFloor paren =
  case paren of
    "(" -> 1
    ")" -> -1
    _ -> 0


day1part1 : String -> Int
day1part1 input =
  List.foldl (+) floor (parensToValues input)


inBasement : Floor -> Bool
inBasement floor =
  floor == -1


-- Part 2


{--
Recursively tests slices of the string to get to basement, adding a move
each time
--}
checkSliceReachesBasementOrContinue : String -> Int -> Int
checkSliceReachesBasementOrContinue input index =
  if day1part1 (String.slice 0 index input) |> inBasement  then
    index
  else if index > String.length input then
    0
  else
    checkSliceReachesBasementOrContinue input (index + 1)


day1part2 : String -> Int
day1part2 input =
  checkSliceReachesBasementOrContinue input 0
