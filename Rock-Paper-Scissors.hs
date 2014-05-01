module Main where

import Control.Applicative
import Data.Char

{- Logic for this game -}
-- Player or AI can make any of these moves each turn
data Move = Rock | Paper | Scissors deriving (Show)

-- Player is the current person playing, and AI is our intelligent program!
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

-- handle invalid cases
convertToMove :: String -> Either String Move
convertToMove input = convert $ map toLower input
  where convert "r" = Right Rock
        convert "s" = Right Scissors
        convert "p" = Right Paper
        convert _   = Left "I don't know that move!"

-- Gives the correct anouncement String for different outcomes of the game
announceWinner :: Winner -> String
announceWinner AI     = "The AI won :)"
announceWinner Draw   = "It was a draw!"
announceWinner Player = "Yay you won!"

badMove :: String
badMove = "Unknown move! " ++ "R for rock, P for paper, S for scissors."
{- END UI/Interaction for the game -}

getResponse :: String -> IO String
getResponse s = putStrLn s >> getLine

getValidMove :: IO Move
getValidMove = do
  userMove <- convertToMove <$> getResponse "What's your move?"
  case userMove of
    Left msg -> do
      putStrLn msg
      putStrLn "R for rock, P for paper, S for scissors."
      getValidMove
    Right m  -> return m

game :: IO ()
game = do
  userMove <- getValidMove
  play userMove makeAIMove
  choice <- getResponse "Continue? Y/N"
  continue choice
    where play m ai = putStrLn $ announceWinner $ getWinner m ai
          continue "y" = game
          continue _ = do putStrLn "Thanks for playing!"

main :: IO ()
main = do
  -- Message to user when the user first runs this program
  putStrLn "Lets play Rock Paper Scissors"
  -- Instructions on ohw to play this game
  putStrLn "R for rock, P for paper, S for scissors."
  game
  
