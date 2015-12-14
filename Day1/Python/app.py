# Importing required modules
import sys

# Creatinging variables
fileName = "text.txt"
floor = 0
currentChar = 1
basementCharFound = False
upFloorChar = "("
downFloorChar = ")"

# Opening file
textVar = open(fileName, 'r')

# Looping through all characters
while True:
    # Reading single character
    char = textVar.read(1)

    # if not at end of file
    if char == upFloorChar:
        # Incrementing floor counter
        floor +=1

    elif char == downFloorChar:
        # Decrementing floor counter
        floor -=1
    elif char == '':
        # Found eof
        break

    # Checking if in basement
    if not basementCharFound:
        if floor == -1:
            print("Santa will go into the basement on character: %s" % (currentChar))
            basementCharFound = True

    # Incrementing char counter
    currentChar += 1

print("Santa will end up on floor: %s" % (floor))
