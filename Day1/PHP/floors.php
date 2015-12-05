<?php

$input = file_get_contents("input.txt"); // Get the input (it's a copy of the input from http://adventofcode.com)
$inputMap = str_split($input); // This turns our 7000 characters into an array of 7000 elements (easier to loop through)

$floor = 0; // Santa starts on the ground floor, which in every non-US country is considered floor 0

$basementEntered = false; // We'll use this to find if we have already entered the basement of the building. (gone negative)

$basementCharacter = "unknown";

for($iteration = 0; $iteration < count($inputMap); $iteration++) {
    if($inputMap[$iteration] == '(') $floor++;
    else if($inputMap[$iteration] == ')') $floor--;
    else die('something strange happened!'); // This just fires off if it encounters a non-parenthesies character.
    // It's just a little bit of sanity checking.

    if($floor < 0 && $basementEntered == false) {
        // If our current floor is less than zero, we're in the basement
        // In addition, We only want to know if it's our first time in the basement. So, we set the $BasementEntered
        // to tell us whether or not we've entered into it already. That way, we remember at what point was the first
        // and don't overwrite it with new data when we enter the basement again.
        $basementCharacter = $iteration; // Set the basement character so we know at what point Santa entered the basement.K
        $basementEntered = true;
    }
}

echo "We finished on floor ".$floor."\n";
echo "And we first entered the basement on instruction ".$basementCharacter;