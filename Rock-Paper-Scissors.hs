module Main where

import Control.Applicative

type Result = Bool

data Move = Rock | Paper | Scissors deriving (Show)

data Winner = Player | AI | Draw deriving (Show)

welcomeMessage :: String
welcomeMessage = "Lets play Rock Paper Scissors"

instructions :: String
instructions = "R for rock, P for paper, S for scissors."

prompt :: String
prompt = "What's your move?"

stringToMove :: String -> Move
stringToMove "r" = Rock
stringToMove "p" = Paper
stringToMove "s" = Scissors
stringToMove _   = Rock

getWinner :: Move -> Move -> Winner
getWinner Rock Paper     = AI
getWinner Rock Scissors  = Player
getWinner Paper Scissors = AI
getWinner Paper Rock     = Player
getWinner Scissors Rock  = AI
getWinner Scissors Paper = Player
getWinner _        _     = Draw

announceWinner :: Winner -> IO ()
announceWinner Draw = putStrLn "It was a draw!"
announceWinner Player = putStrLn "Yay you won!"
announceWinner AI = putStrLn "The AI won :)"

makeAIMove :: Move
makeAIMove = Paper

turn :: Move -> Result
turn = undefined

main :: IO ()
main = do
         putStrLn welcomeMessage
         putStrLn instructions
         putStrLn prompt
         userMove <- stringToMove <$> getLine
         let winner = getWinner userMove makeAIMove
             in announceWinner winner
         putStrLn "hey"
