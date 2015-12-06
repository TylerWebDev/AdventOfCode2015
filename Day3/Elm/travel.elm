import String
import Task
import Http
import Html
import Set


-- main/view


main : Signal Html.Html
main =
  Signal.map view inputMailbox.signal


view : String -> Html.Html
view input =
  Html.div []
    [ Html.div [] [ Html.text ("Day 3 Part 1: " ++ (toString <| day2part1 input)) ]
    , Html.div [] [ Html.text ("Day 3 Part 2: " ++ (toString <| day2part2 input)) ]
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


type alias Location = (Int, Int)


location : Location
location =
  (0, 0)


getNewLocationFromSymbol : Location -> String -> Location
getNewLocationFromSymbol location string =
  case string of
    "^" ->
      (fst location, snd location + 1)
    "<" ->
      (fst location - 1, snd location)
    ">" ->
      (fst location + 1, snd location)
    "v" ->
      (fst location, snd location - 1)
    _ ->
      location


headDefaultStartingLocation : List Location -> Location
headDefaultStartingLocation =
  List.head >> Maybe.withDefault location


appendNewLocationFromSymbolToLocations : String -> List Location -> List Location
appendNewLocationFromSymbolToLocations symbol locations =
  locations ++ [getNewLocationFromSymbol (headDefaultStartingLocation <| List.reverse locations) symbol]


getAllLocationsFromInputSymbolList : List String -> List Location
getAllLocationsFromInputSymbolList input =
  List.foldl appendNewLocationFromSymbolToLocations [] input


day2part1 : String -> Int
day2part1 input =
  Set.size <| Set.fromList <| getAllLocationsFromInputSymbolList (String.split "" input)


-- Part 2


zip : List a -> List b -> List (a,b)
zip = List.map2 (,)


isEven : Int -> Bool
isEven x =
  x % 2 == 0


day2part2 : String -> Int
day2part2 input =
  let
    directions = String.split "" input
    len = String.length input
  in
    Set.size <| Set.fromList <| (\x -> [location] ++ x) <|
    ((getAllLocationsFromInputSymbolList <| snd <| List.unzip <| List.filter (\x -> isEven <| fst x) <| zip [0..len] directions)
    ++
    (getAllLocationsFromInputSymbolList <| snd <| List.unzip <| List.filter (\x -> not (isEven <| fst x)) <| zip [0..len] directions))
