import String
import Task
import Http
import Html
import Array


-- main/view


main : Signal Html.Html
main =
  Signal.map view inputMailbox.signal


view : String -> Html.Html
view input =
  Html.div []
    [ Html.div [] [ Html.text ("Day 5 Part 1: " ++ (toString <| day5part1 input)) ]
    , Html.div [] [ Html.text ("Day 5 Part 2: " ++ (toString <| day5part2 input)) ]
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


isVowel : String -> Bool
isVowel letter =
  not <| List.isEmpty <| List.filter (\x -> x == letter) ["a", "e", "i", "o", "u"]


has3Vowels : String -> Bool
has3Vowels word =
  (\list -> List.length list >= 3) <| List.filter isVowel <| String.split "" word


doesNotContainNaughtyPairs : String -> Bool
doesNotContainNaughtyPairs word =
  letterPairsDoNotContainNaughtyPairs <| getOffsetLetterPairings word


letterPairsDoNotContainNaughtyPairs : List (String, String) -> Bool
letterPairsDoNotContainNaughtyPairs letterPairs =
  List.isEmpty <| List.filter (\(a, b) -> (a ++ b) == "ab" || (a ++ b) == "cd" || (a ++ b) == "pq" || (a ++ b) == "xy") letterPairs


isNicePart1 : String -> Bool
isNicePart1 word =
  has3Vowels word && hasASequentialPairOfLetters word && doesNotContainNaughtyPairs word


zip : List a -> List a -> List (a, a)
zip = List.map2 (,)


hasASequentialPairOfLetters : String -> Bool
hasASequentialPairOfLetters word =
  not <| List.isEmpty <| List.filter (\(x, y) -> x == y ) <| getOffsetLetterPairings word

--  aeidd
-- aeidd
-- ae ei id dd
getOffsetLetterPairings : String -> List (String, String)
getOffsetLetterPairings word =
  zip (String.split "" word) (String.split "" <| String.slice 1 (String.length word) word)

day5part1 : String -> Int
day5part1 input =
  List.length <| List.filter isNicePart1 (String.split "\n" input)


-- Part 2
hasDuplicateButNonAdjacentPairOfLetters : String -> Bool
hasDuplicateButNonAdjacentPairOfLetters input =
  hasDuplicateButNonAdjacentPair (getOffsetLetterPairingsIndexed input) 0


getOffsetLetterPairingsIndexed : String -> List (Int, (String, String))
getOffsetLetterPairingsIndexed word =
  List.indexedMap (,) <| getOffsetLetterPairings word


hasDuplicateButNonAdjacentPair : List (Int, (String, String)) -> Int -> Bool
hasDuplicateButNonAdjacentPair list startIndex =
  if startIndex > List.length list then
    False
  else
    let
      array = Array.fromList list
      length = Array.length array
      nextIndex = startIndex + 1
      prevIndex = (if startIndex - 1 < 0 then 0 else startIndex - 1)
      test = Maybe.withDefault (0, ("", "")) <| Array.get startIndex array
    in
      if Array.isEmpty
          <| Array.filter (\(index, (a, b)) -> (a ++ b == ((\(c, d) -> c ++ d) <| snd test)) && (abs <| nextIndex - index) > 1)
          <| Array.append (if prevIndex /= 0 then Array.slice 0 prevIndex array else Array.fromList []) (Array.slice nextIndex length array)
      then
        hasDuplicateButNonAdjacentPair list nextIndex
      else
        True

-- pairs (x, y), (y, x) recursive check current with next where (a, b) (c, d) | b == c
containsAtLeast1LetterWhichRepeatsWith1LetterBetweenThem : Array.Array (String, String) -> Int -> Bool
containsAtLeast1LetterWhichRepeatsWith1LetterBetweenThem array index =
  if (index + 1) > Array.length array then
    False
  else
    let
      nextIndex = index + 1
      first = Maybe.withDefault ("", "") <| Array.get index array
      second = Maybe.withDefault ("", "") <| Array.get nextIndex array
    in
      if fst first == snd second && snd first == fst second then
        True
      else
        containsAtLeast1LetterWhichRepeatsWith1LetterBetweenThem array nextIndex


isNicePart2 : String -> Bool
isNicePart2 word =
  (containsAtLeast1LetterWhichRepeatsWith1LetterBetweenThem (Array.fromList <| getOffsetLetterPairings word) 0)
  &&
  hasDuplicateButNonAdjacentPairOfLetters word


day5part2 : String -> Int
day5part2 input =
  List.length <| List.filter isNicePart2 (String.split "\n" input)
