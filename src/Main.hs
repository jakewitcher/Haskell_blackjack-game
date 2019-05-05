module Main where

import GameUI

main :: IO ()
main = do
  game <- startGame
  beginRound game 1
  endGame
  return ()

endGame :: IO ()
endGame = do
  putStrLn "Would you like to play again? (Y) (N)"
  response <- getLine
  case response of
    "Y" -> main
    "N" -> endProgram