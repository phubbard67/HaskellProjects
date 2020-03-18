# Not So Colossal Haskel Adventure
This is a somewhat clone of the original game Colossal Cave Adventure orginally made by Will Crowther. This version is ONLY MEANT TO RUN ON LINUX and has been tested on Ubuntu. The game DOES NOT trim any spaces, so 'quit' and '  quit  ' are two different commands. 
## Building the Project
To build the project, navagate to the ColossalCaveAdventure folder and run `stack build`
## Running the Project
To run the project, navagate to the ColossalCaveAdventure folder and run `stack run`
## Written tests
### Test to see that the maze is solvable and game has an end state. 
-- $ stack run
<br>-- press 'enter' key
<br>-- $ left
<br>-- $ left
<br>-- $ left
<br>-- $ right
<br>-- $ left
<br>-- press 'enter' key
<br>-- $ quit

### Test each command
-- stack run
<br>-- $ enter
<br>-- $ inventory
<br>-- $ help
<br>-- $ location
<br>-- $ left
<br>-- $ right
<br>-- $ east
<br>-- $ west
<br>-- $ quit
<br>-- $ quit

### Test that inventory updates
-- stack run
<br>-- hit 'enter' key
<br>-- $ inventory
<br>-- $ left
<br>-- $ left
<br>-- $ inventory
<br>-- $ quit
<br>-- $ quit

### Test that you need the key to win
-- stack run
<br>-- hit 'enter' key
<br>-- $ right 
<br>-- $ right 
<br>-- $ left : Should see that you can't enter the cave
<br>-- $ right 
<br>-- $ left
<br>-- $ left
<br>-- $ inventory : Should see you have the key
<br>-- $ left
<br>-- $ right
<br>-- $ left
<br>-- press 'enter' key
<br>-- $ quit
