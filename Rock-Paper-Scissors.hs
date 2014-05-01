module Main where

import Control.Applicative
import Data.Char

{- Logic for this game -}
-- Player or AI can make any of these moves each turn
data Move = Rock | Paper | Scissors deriving (Show)

data Winner = Player | AI | Draw deriving (Show)

-- use a throwaway function getWinner' because
-- we want to be clear that `user' is the first parameter
getWinner :: Move -> Move -> Winner
getWinner user ai = getWinner' user ai
  where getWinner' Rock Paper     = AI
        getWinner' Rock Scissors  = Player
        getWinner' Paper Scissors = AI
        getWinner' Paper Rock     = Player
        getWinner' Scissors Rock  = AI
        getWinner' Scissors Paper = Player
        getWinner' _        _     = Draw

makeAIMove :: Move
makeAIMove = Paper
{- END Logic for this game -}

{- UI/Interaction for the game -}
-- Message to user when the user first runs this program
welcomeMessage :: String
welcomeMessage = "Lets play Rock Paper Scissors"

-- Instructions on ohw to play this game
instructions :: String
instructions = "R for rock, P for paper, S for scissors."

-- A prompt for the user's input
prompt :: String
prompt = "What's your move?"

-- handle invalid cases
convertToMove :: String -> Maybe Move
convertToMove input = convert $ map toLower input
  where convert "r" = Just Rock
        convert "s" = Just Scissors
        convert "p" = Just Paper
        convert _   = Nothing

-- Gives the correct anouncement String for different outcomes of the game
announceWinner :: Winner -> String
announceWinner Draw   = "It was a draw!"
announceWinner Player = "Yay you won!"
announceWinner AI     = "The AI won :)"

badMove :: String
badMove = "Unknown move! " ++ instructions
{- END UI/Interaction for the game -}

getValidMove :: IO Move
getValidMove = do
  putStrLn prompt
  userMove <- convertToMove <$> getLine
  case userMove of
    Nothing -> do putStrLn badMove; getValidMove
    Just m  -> return m
  

game :: IO ()
game = do
  userMove <- getValidMove
  putStrLn $ play userMove makeAIMove
  putStrLn "continue? Y/N"
  choice <- getLine
  continue choice
    where play m ai = announceWinner $ getWinner m ai
          continue "y" = game
          continue "n" = do putStrLn "Thanks for playing!"

main :: IO ()
main = do
  putStrLn welcomeMessage
  putStrLn instructions
  game
  
