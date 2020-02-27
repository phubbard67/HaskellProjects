main = do putStrLn "              Welcome to Colossal Haskell Adventure"
          putStrLn "   Based on Colossal Cave Adventure created by Will Crowther"
          putStrLn ""
          putStrLn "To play the game, type short phrases into the command line below. \nIf you type the word 'look,' the game gives you a description of your surroundings.  \nTyping 'inventory' tells you what you're carrying.  \n'Get' 'drop' and 'throw' helps you interact with objects.  \nPart of the game is trying out different commands and seeing what happens.  \nType 'help' at any time for game instructions. \n\nWould you like more instructions?"
          response <- getLine
          putStrLn response