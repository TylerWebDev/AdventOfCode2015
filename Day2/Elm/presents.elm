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
    [ Html.div [] [ Html.text ("Day 2 Part 1: " ++ (toString <| day2part1 input)) ]
    , Html.div [] [ Html.text ("Day 2 Part 2: " ++ (toString <| day2part2 input)) ]
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
type alias Dimensions =
  { length : Int
  , width : Int
  , height : Int
  }


dimensions : Dimensions
dimensions =
  { length = 0
  , width = 0
  , height = 0
  }


getRequiredSquareFeetOfPaper : Dimensions -> Int
getRequiredSquareFeetOfPaper dimensions =
  (surfaceArea dimensions) + (getRequiredSlack dimensions)


getRequiredSlack : Dimensions -> Int
getRequiredSlack dimensions =
  default0Minimum [dimensions.length * dimensions.width, dimensions.width * dimensions.height, dimensions.height * dimensions.length]


surfaceArea : Dimensions -> Int
surfaceArea dimensions =
  2 * dimensions.length * dimensions.width + 2 * dimensions.width * dimensions.height + 2 * dimensions.height * dimensions.length


default0Minimum : List number -> number
default0Minimum =
  List.minimum >> Maybe.withDefault 0


default0Head : List number -> number
default0Head =
  List.head >> Maybe.withDefault 0


defaultEmptyTail : List number -> List number
defaultEmptyTail =
  List.tail >> Maybe.withDefault []


listToDimensions : List Int -> Dimensions
listToDimensions list =
  { dimensions
    | length = default0Head list
    , width = default0Head (defaultEmptyTail list)
    , height = default0Head (defaultEmptyTail (defaultEmptyTail list))
  }


safeToInt : String -> Int
safeToInt =
  String.toInt >> Result.toMaybe >> Maybe.withDefault 0


getDimensionsFromStringFormat : String -> Dimensions
getDimensionsFromStringFormat dimensions =
  listToDimensions <| List.map safeToInt (String.split "x" dimensions)


getSquareFootFromStringDimensions : String -> Int
getSquareFootFromStringDimensions string =
  getRequiredSquareFeetOfPaper <| getDimensionsFromStringFormat string


day2part1 : String -> Int
day2part1 input =
  List.sum <|  List.map getSquareFootFromStringDimensions (String.split "\n" input)


-- Part 2


getTotalRibbonRequiredForDimensions : Dimensions -> Int
getTotalRibbonRequiredForDimensions dimensions =
  (getRibbonRequiredForPresent dimensions) + (getRibbonRequiredForBow dimensions)


getRibbonRequiredForPresent : Dimensions -> Int
getRibbonRequiredForPresent dimensions =
  let
    twoShortest = defaultEmptyTail <| List.reverse <| List.sort [dimensions.length, dimensions.width, dimensions.height]
    a = default0Head twoShortest
    b = default0Head <| defaultEmptyTail twoShortest
  in
    2 * a + 2 * b


getRibbonRequiredForBow : Dimensions -> Int
getRibbonRequiredForBow dimensions =
  dimensions.width * dimensions.length * dimensions.height


day2part2 : String -> Int
day2part2 input =
  List.sum <|  List.map (getTotalRibbonRequiredForDimensions << getDimensionsFromStringFormat) (String.split "\n" input)
