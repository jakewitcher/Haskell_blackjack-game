module Main where

import Data.List (transpose)
import System.Random (randomRIO)  
import GameTypes

main :: IO ()
main = do
  game <- startGame
  beginRound game 1
  return ()

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

scoreCard :: Card -> Integer 
scoreCard (Card _ rank) =
  case rank of
    Ace   -> 1
    Two   -> 2
    Three -> 3
    Four  -> 4
    Five  -> 5
    Six   -> 6
    Seven -> 7
    Eight -> 8
    Nine  -> 9
    Ten   -> 10
    Jack  -> 10
    Queen -> 10
    King  -> 10

scorePlayer :: Player -> Player
scorePlayer (Player id h@(Hand cards) _) =
  Player id h s 
  where s = foldr (\c a -> (+ a) $ scoreCard c) 0 cards

scoreAllPlayers :: Game -> Game 
scoreAllPlayers (Game deck players) =
  Game deck (map scorePlayer players)

setupGame :: Deck -> Integer -> IO Game
setupGame deck playerCount = do
  d <- (shuffleDeck deck)
  return $ (scoreAllPlayers . firstHand) (Game d ps)
  where ps = createPlayers playerCount

startGame :: IO Game
startGame = do
  putStrLn "Welcome to BlackJack"
  putStrLn "How many players?"
  playerCount <- getLine
  let x = read playerCount :: Integer
    in setupGame deck x

showPlayerCards :: Game -> Id -> String
showPlayerCards (Game _ players) id =
  show cards
  where (Player _ (Hand cs) _) = 
          head $ filter (\(Player i _ _) -> i == id) players
        cards = foldr (\(Card suit rank) s -> (show rank) ++ " of " ++ (show suit) ++ ", " ++ s) "" cs


showPlayerScore :: Game -> Id -> String
showPlayerScore (Game _ players) id =
  show score
  where (Player _ _ score) = 
          head $ filter (\(Player i _ _) -> i == id) players

beginRound :: Game -> Id -> IO Game 
beginRound game playerId = do
  putStrLn "========================="
  putStrLn $ "Ready player " ++ (show playerId)
  putStrLn "========================="
  putStrLn $ "Your current cards are " ++ (showPlayerCards game playerId)
  putStrLn $ "And your current score is " ++ (showPlayerScore game playerId)
  putStrLn "What would you like to do --"
  putStrLn "(H)it or (S)tand?"
  playerAction <- getLine
  case playerAction of
    "H" -> beginRound (scoreAllPlayers $ dealCard game playerId) playerId
    "S" -> beginRound game (playerId + 1)
    _   -> beginRound game playerId