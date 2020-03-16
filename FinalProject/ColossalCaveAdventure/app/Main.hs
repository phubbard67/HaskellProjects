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
helpString = "To move, try words like left, right, east and west.\n\
\To quit to the main menu type 'quit'.\n\
\Typing 'inventory' tells you what you're carrying.\n\
\Typing 'location' tells you where you are currently.\n\
\The entire point is to get to the cave in the least amount of moves.\n\
\Good luck!\n"


-- This is just the intro string that is partially from https://www.amc.com/shows/halt-and-catch-fire/exclusives/colossal-cave-adventure
gameIntro = "              Welcome to Not So Colossal Haskell Adventure\n\
            \     Based slightly on Colossal Cave Adventure created by Will Crowther\n\n\
            \To play the game, type short phrases into the command line below.\n\ 
            \If you type the word 'quit' during the game you can get back to this menu.\n\
            \Typing 'inventory' tells you what you're carrying.\n\
            \Typing 'location' tells you where you are currently.\n\
            \Part of the game is trying out different commands and seeing what happens.\n\
            \Type 'help' at any time for game instructions.\n\n\
            \To start the game, HIT ENTER and to quit the game type QUIT.\n\
            \ Type HELP at any time to get more instructions. Good Luck!\n"

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
                        putStrLn "You've been told there is gold in a cave near by,\nand you are on a road that seems to stretch on forever \nnext to a river with salmon that smile at you \nand a valley that makes you re-think your life choices.\n"
                        let inventory = "none"
                        game (playerPos , 0, inventory)
                        main
                else return()


        
-- clears the terminal
clear = putStr "\ESC[2J"

-- this is the map for the game
gameMap = [ (("road that seems to stretch on forever", "none") , (1, 4)) , (("river with salmon that smile at you", "none") , (2, 0)) , (("sad looking house", "key") , (4, 1)), (("cave (probably the one with gold)", "door") , (1, 0)) , (("valley that makes you re-think your life choices", "none") , (0, 5)) , (("liquor store for dragons", "none") , (3, 6)) , (("big and tall robe store for plus size wizards", "none") , (5, 7)), (("Betty White secret fight club night club", "none") , (6, 8)) , (("mullet festival", "none") , (6, 4))]


-- This command processes the user input and returns the next location
processCommand :: (String , Maybe Int, Int, String) -> ((String , String) , (Int , Int))
processCommand (a , c, b, d) 
                        | (map toUpper a) == "HELP" = ((helpString , "none") , (-1, -1))
                        | (map toUpper a) == "INVENTORY" = ((getInv d, "none"), (-1, -1))
                        | (map toUpper a) == "LOCATION" = ((("You are at the " ++ getCurrLoc (b) ++ "\nand next to you is a " ++ getMapLeft (b) ++ "\nand a " ++ getMapRight(b)), getMapItem(b)) , (getMapLeftLoc(b), getMapRightLoc(b)))
                        | b == -2 = (("Sorry, that command is not recognized -2" , "none") , (-1, -1))
                        | isInputValid (a, b) = if getMapItem (b) == "none" then ((("You are at the " ++ getCurrLoc (b) ++ "\nand next to you is a " ++ getMapLeft (b) ++ "\nand a " ++ getMapRight(b)) , getMapItem(b)) , (getMapLeftLoc(b), getMapRightLoc(b)))
                                        else if getMapItem(b) == "key" then ((("You are next to a " ++ getMapLeft (b) ++ "\nand a " ++ getMapRight(b) ++ "\nand you have found a " ++ getMapItem(b) ++ " in the house.") , getMapItem(b)) , (getMapLeftLoc(b), getMapRightLoc(b)))
                                        else if getMapItem(b) == "door" && c /= Nothing then (((winString) , getMapItem(b)) , (getMapLeftLoc(b), getMapRightLoc(b)))
                                        else if getMapItem(b) == "door" && c == Nothing then ((("You are next to a " ++ getMapLeft (b) ++ "\nand a " ++ getMapRight(b) ++ ".\nYou see a key hole in the door to the cave, \nbut have no key. It must be somewhere...") , getMapItem(b)) , (getMapLeftLoc(b), getMapRightLoc(b)))
                                        else ((("You are at the " ++ getCurrLoc (b) ++ " and next to you is a " ++ getMapLeft (b) ++ " and a " ++ getMapRight(b)) , getMapItem(b)) , (getMapLeftLoc(b), getMapRightLoc(b)))
                        | otherwise = (("Sorry, that command is not recognized" , "none") , (-1, -1))

                                        
-- This is the main game function
-- It recurses passing in the users postion in the game map
-- and the users inventory. 
game :: (Int , Int, String) -> IO()
game (pos , turns, inv) = do
        response <- getLine
        when (response /= "quit")
                $ do
                        let nextPos = (getNextLocation (response ,  pos)) --try two
                        let location = (processCommand (response , findString "key" inv, nextPos, inv)) --try one
                        putStrLn ((fst ( fst (location))) ++ "\n Turn Number: " ++ show turns ++ "\n")
                        let inventory = (snd (fst (location))) ++ inv 
                        if ((fst ( fst (location))) /= winString) then do
                                game (nextPos , (turns + 1), inventory)
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

-- returns the current location string
getCurrLoc :: Int -> String
getCurrLoc a = fst ( fst(gameMap !! a))

-- returns the key if in inventory, empty if not
getInv :: String -> String
getInv a | findString "key" a /= Nothing = "you have a key"
         | otherwise = "Pockets are empty, but hopes are high."

-- This returns the next list location from the string input by the user
getNextLocation :: (String , Int) ->  Int
getNextLocation (a , b) = if (map toUpper a) == "LEFT"  then (getMapLeftLoc(b))
                    else if (map toUpper a) == "EAST" then (getMapLeftLoc(b))
                    else if (map toUpper a) == "WEST" then (getMapRightLoc(b))
                    else if (map toUpper a) == "RIGHT" then (getMapRightLoc(b))
                    else b

-- This returns true if the user input is a valid input
isInputValid :: (String, Int) -> Bool
isInputValid (a, b) | (map toUpper a) == "LEFT" = True
                    | (map toUpper a) == "EAST" = True
                    | (map toUpper a) == "WEST" = True
                    | (map toUpper a) == "RIGHT" = True
                    | (map toUpper a) == "NONE" = True
                    | otherwise = False


-- Test to see that the maze is solvable and game has an end state. 
-- $ stack run
-- press 'enter' key
-- $ left
-- $ left
-- $ right
-- $ left
-- $ left
-- $ right
-- $ left
-- press 'enter' key
-- $ quit

-- test each command
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

-- Test that inventory updates
-- stack run
-- hit 'enter' key
-- $ inventory
-- $ left
-- $ left
-- $ inventory
-- $ quit
-- $ quit

-- Test that you need the key to win
-- stack run
-- hit 'enter' key
-- 