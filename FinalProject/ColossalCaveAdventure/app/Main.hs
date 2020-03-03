module Main where

main :: IO ()
main =  do
        clear
        putStrLn gameIntro
        response <- getLine
        putStrLn (processCommand response)
        
-- clears the terminal
clear = putStr "\ESC[2J"

-- TODO: Maybe turn the game map into a type
-- this is the map for the game
gameMap = [("road", (1, 2)),("river", (0, 2)), ("house", (0, 1))]

-- TODO: Change this so it takes in the game map position
--        so that you can track where the user is in the game
--        the main loop should hold an updated list position var
--        that gets passed in, and then you can reference the game map
--        based on this value. Where it says gameMap !! 0 it will read
--        gameMap !! mapPos
-- This command processes the user input and returns the next location
processCommand :: String -> String
processCommand a = if a == "help" then helpString 
                    else if a == "road" then ("You are next to a " ++ (fst (gameMap !! (fst (snd (gameMap !! 0))))))
                    else "Sorry, that command is not recognized"


-- This is just the string for help that is from https://www.amc.com/shows/halt-and-catch-fire/exclusives/colossal-cave-adventure
helpString = "I know of places, actions, and things.  Most of my vocabulary describes\n\
\places and is used to move you there.  To move, try words like forest,\n\
\building, downstream, enter, east, west, north, south, up or down.\n\
\I know about a few special objects, like a black rod hidden in the cave.\n\
\These objects can be manipulated using some of the action words I know.\n\
\Usually you will need to give both the object and action words -In\n\
\either order - but sometimes I can infer the object from the verb alone.\n\
\Some objects also imply verbs; in particular, 'inventory' implies 'take\n\
\inventory,' which causes me to give you a list of what you're carrying.\n\
\The objects have side effects; for instance, the rod scares the bird.\n\
\Usually people having trouble moving just need to try a few more words.\n\
\Usually people trying unsuccessfully to manipulate an object are\n\
\attempting something beyond their (or my!) capabilities and should try a\n\
\completely different tack.  To speed the game you can sometimes move long\n\
\distances with a single word.  For example, 'building' usually gets you to\n\
\the building from anywhere above ground except when lost in the forest.\n\
\Also, note that cave passages turn a lot, and that leaving a room to\n\
\the north does not guarantee entering the next from the south.  Good luck!"

gameIntro = "              Welcome to Colossal Haskell Adventure\n\
            \     Based on Colossal Cave Adventure created by Will Crowther\n\n\
            \To play the game, type short phrases into the command line below.\n\ 
            \If you type the word 'look,' the game gives you a description of your surroundings.\n\
            \Typing 'inventory' tells you what you're carrying.\n\
            \'Get' 'drop' and 'throw' helps you interact with objects.\n\
            \Part of the game is trying out different commands and seeing what happens.\n\
            \Type 'help' at any time for game instructions.\n\n\
            \Would you like more instructions?"