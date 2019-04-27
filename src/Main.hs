module Main where

import System.Random (randomRIO)  
import GameTypes

main :: IO ()
main = do
  putStrLn "blackjack"

ranks = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]
suits = [Spade, Club, Heart, Diamond]


makeCards suits ranks =
  foldr (\suit cards -> map (\rank -> go suit rank) ranks ++ cards) [] suits
  where go s r =
          Card s r