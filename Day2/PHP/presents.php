<?php
$file = file('input.txt');

$paper = 0;
$ribbon = 0;

foreach ($file as $dimensions) {
    $box = explode('x', $dimensions);
    sort($box, 1);
    $extra = $box[0]*$box[1];
    $sides = $extra*2;
    $tb = ($box[1]*$box[2])*2;
    $fb = ($box[0]*$box[2])*2;
    $ribbon += ($box[0]*$box[1]*$box[2]) + $box[0]*2 + $box[1]*2;
    $paper += $extra+$sides+$tb+$fb;
}
echo "You will need $paper feet of wrapping paper\nand $ribbon feet of ribbon.";