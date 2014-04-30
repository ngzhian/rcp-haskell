module Main where

import Control.Applicative
import Data.Char
import Data.List as L

type Result = Bool

data Move = Rock | Paper | Scissors deriving (Show)

data Winner = Player | AI | Draw deriving (Show)

welcomeMessage :: String
welcomeMessage = "Lets play Rock Paper Scissors"

instructions :: String
instructions = "R for rock, P for paper, S for scissors."

prompt :: String
prompt = "What's your move?"

convertToMove :: String -> Move
convertToMove input = convert $ L.map toLower input
  where convert "r" = Rock
        convert "s" = Scissors
        convert "p" = Paper

getWinner :: Move -> Move -> Winner
getWinner user ai = getWinner' user ai
  where getWinner' Rock Paper     = AI
        getWinner' Rock Scissors  = Player
        getWinner' Paper Scissors = AI
        getWinner' Paper Rock     = Player
        getWinner' Scissors Rock  = AI
        getWinner' Scissors Paper = Player
        getWinner' _        _     = Draw

announceWinner :: Winner -> String
announceWinner Draw   = "It was a draw!"
announceWinner Player = "Yay you won!"
announceWinner AI     = "The AI won :)"

makeAIMove :: Move
makeAIMove = Paper

main :: IO ()
main = do
  putStrLn welcomeMessage
  putStrLn instructions
  game

game :: IO ()
game = do
  putStrLn prompt
  userMove <- convertToMove <$> getLine
  putStrLn $ announceWinner $ getWinner userMove makeAIMove
  putStrLn "continue? Y/N"
  choice <- getLine
  continue choice
    where continue "y" = game
          continue "n" = do putStrLn "Thanks for playing!"
  
