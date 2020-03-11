module Main where

import Data.Char
import Control.Monad
import Data.IORef

-- TODO: Make this a recursing loop. There is an example of how 
--       to do so in the IO section of learn you a haskell
--       you should also be passing in the gameMap location
--       to processCommand, and updating it each turn
main :: IO ()
main =  do
        clear
        putStrLn gameIntro
        response <- getLine
        if response /= "quit"
                then do
                        game
                        main
                else return()


        
-- clears the terminal
clear = putStr "\ESC[2J"

-- TODO: Maybe turn the game map into a type
-- this is the map for the game
gameMap = [ (("road", "none") , (1, 2)) , (("river", "none") , (3, 0)) , (("house", "key") , (0, 1)), (("cave", "door") , (1, 0))]

-- TODO: Change this so it takes in the game map position
--        so that you can track where the user is in the game
--        the main loop should hold an updated list position var
--        that gets passed in, and then you can reference the game map
--        based on this value. Where it says gameMap !! 0 it will read
--        gameMap !! mapPos
-- This command processes the user input and returns the next location
processCommand :: (String , String, Int) -> ((String , String) , (Int , Int))
processCommand (a , c, b) 
                        | a == "help" = ((helpString , "none") , (-1, -1))
                        | b == -2 = (("Sorry, that command is not recognized" , "none") , (-1, -1))
                        | a == "no"  = if getMapItem (b) == "none" then ((("You are next to a " ++ getMapLeft (b) ++ " and a " ++ getMapRight(b)) , getMapItem(b)) , (getMapLeftLoc(b), getMapRightLoc(b)))
                                        else if getMapItem(b) == "key" then ((("You are next to a " ++ getMapLeft (b) ++ " and a " ++ getMapRight(b) ++ " and you have found a " ++ getMapItem(b) ++ " in the room.") , getMapItem(b)) , (getMapLeftLoc(b), getMapRightLoc(b)))
                                        else if getMapItem(b) == "door" && c == "key" then ((("You are at the cave and have found the key! Congratulations, you now have all the gold!") , getMapItem(b)) , (getMapLeftLoc(b), getMapRightLoc(b)))
                                        else if getMapItem(b) == "door" && c == "none" then ((("You are next to a " ++ getMapLeft (b) ++ " and a " ++ getMapRight(b) ++ ". You see a key hole, but have no key. It must be somewhere...") , getMapItem(b)) , (getMapLeftLoc(b), getMapRightLoc(b)))
                                        else ((("You are next to a " ++ getMapLeft (b) ++ " and a " ++ getMapRight(b)) , getMapItem(b)) , (getMapLeftLoc(b), getMapRightLoc(b)))
                        | otherwise = (("Sorry, that command is not recognized" , "none") , (-1, -1))

                                        
-- TODO: return if the player has the winning string
game :: IO()
game = do
        when (response /= "quit")
                $ do
                        let location = (processCommand (response , "none", playerPos)) --try one
                        putStrLn (fst ( fst (location)))
                        let inventory = (snd (fst (location))) 
                        response <- getLine
                        let nextPos = (getNextLocation (response ,  playerPos)) --try two 
                        let nextLoc = (processCommand ("no" , searchInv inventory, nextPos))
                        putStrLn (fst ( fst (nextLoc)))
                        let inventoryOne = inventory ++ (snd (fst (nextLoc))) 
                        response <- getLine
                        let nextPosOne = (getNextLocation (response , nextPos)) --try three
                        let nextLocOne = (processCommand ("no" , searchInv inventoryOne, nextPosOne))
                        putStrLn (fst ( fst (nextLocOne)))
                        print nextLocOne
                        let inventoryTwo = inventoryOne ++ (snd (fst (nextLocOne)))
                        response <- getLine
                        let nextPosTwo = (getNextLocation (response , nextPosOne)) --try four
                        let nextLocTwo = (processCommand ("no" , searchInv inventoryTwo, nextPosTwo))
                        putStrLn (fst ( fst (nextLocTwo)))
                        let inventoryThree = inventoryTwo ++ (snd (fst (nextLocTwo)))
                        putStrLn inventoryThree
                        putStrLn (searchInv inventoryThree)
                        response <- getLine
                        let nextPosThree = (getNextLocation (response, nextPosTwo)) --try five
                        let nextLocThree = (processCommand ("no" , searchInv inventoryThree, nextPosThree))
                        putStrLn (fst ( fst (nextLocThree)))
                

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

-- This holds the players position in the list
playerPos = 0

-- used to start the game
response = "no"



-- sets the inventory if the user has a key
searchInv :: String -> String
searchInv a | (filter (/= "key") [a] /= []) = "key"
            | otherwise = "none"

-- This returns the next list location from the string input by the user
getNextLocation :: (String , Int) ->  Int
getNextLocation (a , b) = if (map toUpper a) == "LEFT"  then (getMapLeftLoc(b))
                    else if (map toUpper a) == "RIGHT" then (getMapRightLoc(b))
                    else -2

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

