module Main where

import Data.Char
import Control.Monad
import Data.IORef
import Data.List

-- ///////////////////////////////////////////////////Vars
-- This holds the players position in the list
playerPos = 0

-- This is the string to tell you that you have won
winString = "You are at the cave and have found the key! Congratulations, you now have all the gold!\n Hit the enter key to get to the main menu"


-- This is just the string for help that is from https://www.amc.com/shows/halt-and-catch-fire/exclusives/colossal-cave-adventure
helpString = "To move, try words like left and right.\n\
\To start the game, hit any key. Once in the game\n\
\you cannot leave until you run out of turns or lose. Good luck!"


-- This is just the intro string that is partially from https://www.amc.com/shows/halt-and-catch-fire/exclusives/colossal-cave-adventure
gameIntro = "              Welcome to Colossal Haskell Adventure\n\
            \     Based slightly on Colossal Cave Adventure created by Will Crowther\n\n\
            \To play the game, type short phrases into the command line below.\n\ 
            \If you type the word 'quit,' you can leave the game, but only from this menu.\n\
            \Typing 'inventory' tells you what you're carrying.\n\
            \Part of the game is trying out different commands and seeing what happens.\n\
            \Type 'help' at any time for game instructions.\n\n\
            \To start the game, hit enter and to quit the game type quit.\n\
            \ YOU CANNOT QUIT once the game begins. You get 5 trys to enter the cave. Good Luck!"

-- ////////////////////////////end Vars

-- /////////////////////////////////////////////////////Functions

-- this function was found at https://stackoverflow.com/questions/48198144/how-do-i-search-for-string-within-a-string-in-haskell
-- it gets the index of a string in a string
findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)

-- main calls the game function and is basically the 'main menu' of the game
main :: IO ()
main =  do
        clear
        putStrLn gameIntro
        let playerPos = 0
        response <- getLine
        if response /= "quit"
                then do
                        putStrLn "You've been told there is gold in a cave near by.\n You are next to a river and a house."
                        let inventory = "none"
                        game (playerPos , inventory)
                        main
                else return()


        
-- clears the terminal
clear = putStr "\ESC[2J"

-- this is the map for the game
gameMap = [ (("road", "none") , (1, 2)) , (("river", "none") , (3, 0)) , (("house", "key") , (0, 1)), (("cave", "door") , (1, 0))]


-- This command processes the user input and returns the next location
processCommand :: (String , Maybe Int, Int) -> ((String , String) , (Int , Int))
processCommand (a , c, b) 
                        | a == "help" = ((helpString , "none") , (-1, -1))
                        | b == -2 = (("Sorry, that command is not recognized -2" , "none") , (-1, -1))
                        | isInputValid (a, b) = if getMapItem (b) == "none" then ((("You are next to a " ++ getMapLeft (b) ++ " and a " ++ getMapRight(b)) , getMapItem(b)) , (getMapLeftLoc(b), getMapRightLoc(b)))
                                        else if getMapItem(b) == "key" then ((("You are next to a " ++ getMapLeft (b) ++ " and a " ++ getMapRight(b) ++ " and you have found a " ++ getMapItem(b) ++ " in the house.") , getMapItem(b)) , (getMapLeftLoc(b), getMapRightLoc(b)))
                                        else if getMapItem(b) == "door" && c /= Nothing then (((winString) , getMapItem(b)) , (getMapLeftLoc(b), getMapRightLoc(b)))
                                        else if getMapItem(b) == "door" && c == Nothing then ((("You are next to a " ++ getMapLeft (b) ++ " and a " ++ getMapRight(b) ++ ". You see a key hole, but have no key. It must be somewhere...") , getMapItem(b)) , (getMapLeftLoc(b), getMapRightLoc(b)))
                                        else ((("You are next to a " ++ getMapLeft (b) ++ " and a " ++ getMapRight(b)) , getMapItem(b)) , (getMapLeftLoc(b), getMapRightLoc(b)))
                        | otherwise = (("Sorry, that command is not recognized" , "none") , (-1, -1))

                                        
-- This is the main game function
-- It recurses passing in the users postion in the game map
-- and the users inventory. 
game :: (Int , String) -> IO()
game (pos , inv) = do
        response <- getLine
        when (response /= "quit")
                $ do
                        let nextPos = (getNextLocation (response ,  pos)) --try two
                        let location = (processCommand (response , findString "key" inv, nextPos)) --try one
                        putStrLn (fst ( fst (location)))
                        let inventory = (snd (fst (location))) ++ inv 
                        if ((fst ( fst (location))) /= winString) then do
                                game (nextPos , inventory)
                        else do
                                wait <- getLine
                                return()
                

-- returns the left location string
getMapLeft :: Int -> String
getMapLeft a = (fst ( fst (gameMap !! (fst (snd (gameMap !! a))))))

-- returns the location of the left map in the list
getMapLeftLoc :: Int -> Int
getMapLeftLoc a = (fst (snd (gameMap !! a)))

-- returns the right map location
getMapRight :: Int -> String
getMapRight a = (fst (fst (gameMap !! (snd (snd (gameMap !! a))))))

-- returns the right location in the maps list
getMapRightLoc :: Int -> Int
getMapRightLoc a = (snd (snd (gameMap !! a)))

-- returns the item in the room
getMapItem :: Int -> String
getMapItem a = snd (fst ((gameMap !! a)))

-- This returns the next list location from the string input by the user
getNextLocation :: (String , Int) ->  Int
getNextLocation (a , b) = if (map toUpper a) == "LEFT"  then (getMapLeftLoc(b))
                    else if (map toUpper a) == (map toUpper (getMapLeft(b))) then (getMapLeftLoc(b))
                    else if (map toUpper a) == (map toUpper (getMapRight(b))) then (getMapRightLoc(b))
                    else if (map toUpper a) == "RIGHT" then (getMapRightLoc(b))
                    else -2

-- This returns true if the user input is a valid input
isInputValid :: (String, Int) -> Bool
isInputValid (a, b) | (map toUpper a) == "LEFT" = True
                    | (map toUpper a) == (map toUpper (getMapLeft(b))) = True
                    | (map toUpper a) == (map toUpper (getMapRight(b))) = True
                    | (map toUpper a) == "RIGHT" = True
                    | (map toUpper a) == "NONE" = True
                    | otherwise = False


-- TODO: Add a function that shows your location so you can tell the 
--       player where they are. Add that to the string from processCommand.
--       Edit the help and beginning output. 
--       Test the isInputValid with the actual location names. Get it to work. 

