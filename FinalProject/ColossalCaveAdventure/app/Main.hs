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
gameMap = [ (("road that seems to stretch on forever", "none") , (1, 4)) , (("river with salmon that smile at you", "none") , (2, 0)) , (("sad looking house", "key") , (4, 1)), (("cave (probably the one with gold)", "door") , (1, 0)) , (("valley that makes you re-think your life choices", "none") , (0, 5)) , (("liquor store for dragons", "dragon") , (3, 6)) , (("big and tall robe store for plus size wizards", "none") , (5, 7)), (("Betty White secret fight club night club", "hair") , (6, 8)) , (("mullet festival", "none") , (6, 4))]


-- This command processes the user input and returns the next location
processCommand :: (String , Maybe Int, Int, String) -> ((String , String) , (Int , Int))
processCommand (a , c, b, d) 
                        | (map toUpper a) == "HELP" = ((helpString , "none") , (-1, -1))
                        | (map toUpper a) == "INVENTORY" = ((getInv d, "none"), (-1, -1))
                        | (map toUpper a) == "LOCATION" = ((("You are at the " ++ getCurrLoc (b) ++ "\nand next to you is a " ++ getMapLeft (b) ++ "\nand a " ++ getMapRight(b)), getMapItem(b)) , (getMapLeftLoc(b), getMapRightLoc(b)))
                        | b == -2 = (("Sorry, that command is not recognized -2" , "none") , (-1, -1))
                        | isInputValid (a, b) = if getMapItem (b) == "none" then ((("You are at the " ++ getCurrLoc (b) ++ "\nand next to you is a " ++ getMapLeft (b) ++ "\nand a " ++ getMapRight(b)) , getMapItem(b)) , (getMapLeftLoc(b), getMapRightLoc(b)))
                                        else if getMapItem(b) == "key" then ((("You are next to a " ++ getMapLeft (b) ++ "\nand a " ++ getMapRight(b) ++ "\nand you have found a " ++ getMapItem(b) ++ " in the house. (limit one per person)") , getMapItem(b)) , (getMapLeftLoc(b), getMapRightLoc(b)))
                                        else if getMapItem(b) == "hair" then ((("You are next to a " ++ getMapLeft (b) ++ "\nand a " ++ getMapRight(b) ++ "\nand you have found a lock of Betty White's hair.\nWith this magic item, you might get an extra prize. (limit one per person)") , getMapItem(b)) , (getMapLeftLoc(b), getMapRightLoc(b)))
                                        else if getMapItem(b) == "dragon" then ((("You are next to a " ++ getMapLeft (b) ++ "\nand a " ++ getMapRight(b) ++ "\nand you have found a bottle of 'Dragons Gold'.\nSeems to be a worse version of a liquor with gold flakes in it (if it could be worse).\nWith this magic item, you might get an extra prize. (limit one per person)") , getMapItem(b)) , (getMapLeftLoc(b), getMapRightLoc(b)))
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
                        let nextPos = (getNextLocation (response ,  pos)) 
                        let location = (processCommand (response , findString "key" inv, nextPos, inv)) 
                        putStrLn ((fst ( fst (location))) ++ "\n Turn Number: " ++ show turns ++ "\n")
                        let inventory = (snd (fst (location))) ++ inv 
                        if ((fst ( fst (location))) /= winString) then do
                                game (nextPos , (turns + 1), inventory)
                        else do
                                if (hasHair inv && hasLiquor inv) then do
                                                                putStrLn "Extra Credit:\nYou have the Dragon's Gold liquor! That slowed down time for you, and gave you a better score!\nYou have the Betty White's Hair! The magic of Betty White has turned back time, and gave you a better score!"
                                                                let newTurnsTwo = turns - 3;
                                                                putStrLn ("\nFinal Score: " ++ show newTurnsTwo)
                                                                wait <- getLine
                                                                return()
                                else if hasLiquor inv then do 
                                                        putStrLn "Extra Credit:\nYou have the Dragon's Gold liquor! That slowed down time for you, and gave you a better score!\nLooks like you didn't visit the Betty White Night Club Fight Club. Oh well..."
                                                        let newTurns = turns - 1;
                                                        putStrLn ("\nFinal Score: " ++ show newTurns)
                                                        wait <- getLine
                                                        return()
                                else if hasHair inv then do
                                                        putStrLn "Extra Credit:\nYou have a lock Betty White's Hair! The magic of Betty White has turned back time, and gave you a better score!\nLooks like you didn't go the the Dragon Liquor Store. Oh well..."
                                                        let newTurnsOne = turns - 2;
                                                        putStrLn ("\nFinal Score: " ++ show newTurnsOne)
                                                        wait <- getLine
                                                        return()
                                else do 
                                        putStrLn "Extra Credit:\nLooks like you didn't get the Dragon's Liquor or Betty White's hair. Oh well."
                                        putStrLn ("Final Score: " ++ show turns)
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
getInv a | (findString "key" a /= Nothing) && (findString "dragon" a /= Nothing) &&  (findString "hair" a /= Nothing)= "\nInventory:\nCave key\nBottle of Dragon's Gold liquor\nA lock of Betty White's hair"
         | (findString "key" a /= Nothing) && (findString "dragon" a /= Nothing) = "\nInventory:\nCave key\nBottle of Dragon's Gold liquor"
         | findString "key" a /= Nothing = "\nInventory:\nA cave key"
         | findString "dragon" a /= Nothing = "\nInventory:\nA bottle of Dragon's Gold liquor"
         | findString "hair" a /= Nothing = "\nInventory:\nA lock of Betty White's hair"
         | otherwise = "\nPockets are empty, but hopes are high."

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

-- checks for the dragon liquor
hasLiquor :: String -> Bool
hasLiquor a | findString "dragon" a /= Nothing = True
            | otherwise = False

-- checks for the Betty Whites Hair
hasHair :: String -> Bool
hasHair a | findString "hair" a /= Nothing = True
            | otherwise = False

-- Test to see that the maze is solvable and game has an end state. 
-- $ stack run
-- press 'enter' key
-- $ left
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