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
game = Game deck (createPlayers 4)

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

createPlayers :: Integer -> [Player]
createPlayers 0 = []
createPlayers playerCount =
  createPlayers (playerCount - 1) ++ [Player playerCount (Hand []) 0]

dealCard :: Game -> Id -> Game
dealCard (Game (Deck deck) players) id = 
  Game d (go id $ players)
  where d = Deck $ drop 1 deck
        c = take 1 deck
        go i = map (\p@(Player x (Hand h) s) -> if x == i then Player x (Hand $ c ++ h) s else p)

firstHand :: Game -> Game
firstHand g@(Game _ players) = 
  go (go g c) c
  where c = toInteger $ length players
        go game 0 = game
        go game count =
          go (dealCard game count) (count - 1)

initializeGame :: Deck -> Integer -> IO Game
initializeGame deck playerCount = do
  d <- (shuffleDeck deck)
  return $ firstHand (Game d ps)
  where ps = createPlayers playerCount