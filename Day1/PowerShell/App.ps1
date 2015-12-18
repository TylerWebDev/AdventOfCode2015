# Variables
$fileName = "text.txt"
$floor = 0
$currentChar = 1
$basementFound = $false
$upFloorChar = "("
$downFloorChar = ")"

# Getting file contents
$fileContents = Get-Content .\text.txt

# Looping through file contents
for($i = 0; $i -lt $fileContents.Length; $i++){
    if($fileContents[$i] -eq $upFloorChar){
        $floor++
    }

    elseif($fileContents[$i] -eq $downFloorChar){
        $floor--
    }

    if(-Not $basementFound){

        if($floor -eq -1){
            $basementPrompt = "Santa will go into the basement on character $currentChar"
               
            $basementFound = $true
        }
    }

    $currentChar++
}

Write-Host "Santa will end up on floor: $floor"
Write-Host $basementPrompt