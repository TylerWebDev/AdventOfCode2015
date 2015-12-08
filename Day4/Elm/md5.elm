import Html
import Native.Native
import String


-- main/view


main : Html.Html
main =
  view "ckczppom"


view : String -> Html.Html
view input =
  Html.div []
    [ Html.div [] [ Html.text ("Day 4 Part 1: " ++ (toString <| day2part1 input)) ]
    , Html.div [] [ Html.text ("Day 4 Part 2: " ++ (toString <| day2part2 input)) ]
    ]


-- Part 1


checkHash : String -> Int -> String -> Int
checkHash secret start requirement =
  let
    test = start + 1
    length = String.length requirement
    input = secret ++ (toString test)
    hash = Native.Native.md5 input
  in
    if String.left length hash == requirement then
      test
    else
      checkHash secret test requirement



day2part1 : String -> Int
day2part1 input =
  checkHash input -1 "00000"


-- Part 2


day2part2 : String -> Int
day2part2 input =
  checkHash input -1 "000000"
