# HaskellProjects
These are a few haskell projects from school


# Not So Colossal Haskel Adventure
This is a somewhat clone of the original game Colossal Cave Adventure orginally made by Will Crowther. This version is ONLY MEANT TO RUN ON LINUX and has been tested on Ubuntu. The game DOES NOT trim any spaces, so 'quit' and '  quit  ' are two different commands. 
## Building the Project
To build the project, navagate to the ColossalCaveAdventure folder and run `stack build`
## Running the Project
To run the project, navagate to the ColossalCaveAdventure folder and run `stack run`
## Written tests
### Test to see that the maze is solvable and game has an end state. 
-- $ stack run
-- press 'enter' key
-- $ left
-- $ left
-- $ left
-- $ right
-- $ left
-- press 'enter' key
-- $ quit

### Test each command
-- stack run
-- $ enter
-- $ inventory
-- $ help
-- $ location
-- $ left
-- $ right
-- $ east
-- $ west
-- $ quit
-- $ quit

### Test that inventory updates
-- stack run
-- hit 'enter' key
-- $ inventory
-- $ left
-- $ left
-- $ inventory
-- $ quit
-- $ quit

### Test that you need the key to win
-- stack run
-- hit 'enter' key
-- $ right 
-- $ right 
-- $ left : Should see that you can't enter the cave
-- $ right 
-- $ left
-- $ left
-- $ inventory : Should see you have the key
-- $ left
-- $ right
-- $ left
-- press 'enter' key
-- $ quit
