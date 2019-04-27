module Main where

import Data.List (transpose)
import System.Random (randomRIO)  
import GameTypes

main :: IO ()
main = do
  putStrLn "blackjack"

suits = [Spade, Club, Heart, Diamond]
ranks = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]

makeDeck :: [Suit] -> [Rank] -> Deck
makeDeck suits ranks =
  Deck $ foldr (\suit cards -> map (\rank -> go suit rank) ranks ++ cards) [] suits
  where go s r =
          Card s r

deck = makeDeck suits ranks

cutDeck :: Int -> Deck -> ([Card], [Card])
cutDeck index (Deck d) =
  go $ splitAt index d
  where go (q@(x:xs), t@(y:ys))
          | left == right = (q, t)
          | left > right  = go (xs, x:t)
          | left < right  = go (y:q, ys)
          where left = length q 
                right = length t

bridgeDeck :: ([Card], [Card]) -> Deck
bridgeDeck (a, b) =
  Deck $ concat . transpose $ [a, b]

shuffle :: Deck -> IO Deck
shuffle deck@(Deck d) = do
  randomInt <- randomRIO(1, (length d) - 1)
  return (bridgeDeck $ cutDeck randomInt deck)

shuffleDeck' :: Int -> IO Deck -> IO Deck
shuffleDeck' 0 deck = deck
shuffleDeck' count deck = shuffleDeck' (count - 1) (deck >>= shuffle)

shuffleDeck :: Deck -> IO Deck 
shuffleDeck = (shuffleDeck' 10000) . shuffle

