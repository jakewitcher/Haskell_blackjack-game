module GameUI (startGame
              , beginRound
              , endProgram) where

import System.Exit (exitSuccess)
import GameLogic
import GameTypes

startGame :: IO Game
startGame = do
  putStrLn "Welcome to BlackJack"
  putStrLn "How many players? (Limit: 7)"
  playerCount <- getLine
  let x = read playerCount :: Integer
    in if x > 7 || x < 1
      then startGame
      else setupGame x

beginRound :: Game -> Id -> IO Game
beginRound game@(Game _ players) playerId = do
  putStrLn "\n"
  putStrLn "========================="
  putStrLn $ "Player " ++ (show playerId)
  putStrLn "========================="
  putStrLn $ "Your current cards are " ++ (showPlayerCards game playerId)
  putStrLn $ "And your current score is " ++ (showPlayerScore game playerId)
  putStrLn "\n"
  if checkPlayerScore game playerId
    then continueRoundWithNextPlayer game playerId
    else continueRoundWithCurrentPlayer game playerId

continueRoundWithCurrentPlayer :: Game -> Id -> IO Game 
continueRoundWithCurrentPlayer game@(Game _ players) playerId = do
  putStrLn "What would you like to do --"
  putStrLn "(H)it or (S)tand?"
  playerAction <- getLine
  case playerAction of
    "H" -> beginRound (scoreAllPlayers $ dealCard game playerId) playerId
    "S" -> if checkGameStatus game playerId
            then beginRound game (playerId + 1) 
            else endRound game
    _   -> beginRound game playerId

continueRoundWithNextPlayer :: Game -> Id -> IO Game 
continueRoundWithNextPlayer game id = do 
  putStrLn "Sorry, you went over 21"
  if checkGameStatus game id
    then beginRound (dropPlayer game id) (id + 1)
    else endRound (dropPlayer game id)

endRound :: Game -> IO Game
endRound game = do
  putStrLn $ (announceWinner . determineWinner) game
  return game

announceWinner :: Maybe (Id, Score) -> String
announceWinner m =
  case m of
    Just (id, score) -> "\nThe winner is Player " ++ (show id) ++ " with a score of " ++ (show score) ++ " points!"
    Nothing -> "\nI'm sorry, I guess you are all losers"

endProgram :: IO ()
endProgram = do 
  putStrLn "\nThanks for playing!"
  exitSuccess
  return ()