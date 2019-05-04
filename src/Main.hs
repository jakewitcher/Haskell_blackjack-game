module Main where

import Data.List (transpose,  intercalate)
import System.Random (randomRIO)
import System.Exit (exitSuccess)
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

cutDeck :: Int -> Deck -> ([Card], [Card])
cutDeck index (Deck d) =
  go $ splitAt index d
  where go (a@(x:xs), b@(y:ys))
          | left == right = (a, b)
          | left > right  = go (xs, x:b)
          | left < right  = go (y:a, ys)
          where left = length a
                right = length b

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

scoreCard :: Card -> (Integer, Integer)
scoreCard (Card _ rank) =
  case rank of
    Ace   -> (1, 11)
    Two   -> (2, 2)
    Three -> (3, 3)
    Four  -> (4, 4)
    Five  -> (5, 5)
    Six   -> (6, 6)
    Seven -> (7, 7)
    Eight -> (8, 8)
    Nine  -> (9, 9)
    _     -> (10, 10)

scorePlayer :: Player -> Player
scorePlayer (Player id h@(Hand cards) _) =
  Player id h s
  where temp = foldr go 0 cards
        s = if temp > 21 then foldr go' 0 cards else temp
        go c a = (+a) (snd $ scoreCard c)
        go' c a = (+a) (fst $ scoreCard c)

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
  putStrLn "How many players? (Limit: 7)"
  playerCount <- getLine
  let x = read playerCount :: Integer
    in if x > 7 || x < 1
      then startGame
      else setupGame deck x

showPlayerCards :: Game -> Id -> String
showPlayerCards (Game _ players) id =
  intercalate ", " cards
  where (Player _ (Hand cs) _) = 
          head $ filter (\(Player i _ _) -> i == id) players
        cards = foldr (\(Card suit rank) s -> (:s) $ (show rank) ++ " of " ++ (show suit) ++ "s") [] cs

showPlayerScore :: Game -> Id -> String
showPlayerScore (Game _ players) id =
  show score
  where (Player _ _ score) = 
          head $ filter (\(Player i _ _) -> i == id) players

getPlayerScore :: Game -> Id -> Score 
getPlayerScore (Game _ players) id =
  score
  where (Player _ _ score) = 
          head $ filter (\(Player i _ _) -> i == id) players

beginRound :: Game -> Id -> IO Game
beginRound game@(Game _ players) playerId = do
  putStrLn "========================="
  putStrLn $ "Ready player " ++ (show playerId)
  putStrLn "========================="
  putStrLn $ "Your current cards are " ++ (showPlayerCards game playerId)
  putStrLn $ "And your current score is " ++ (showPlayerScore game playerId)
  if (getPlayerScore game playerId) > 21
    then continueRoundWithNextPlayer game playerId
    else continueRoundWithCurrentPlayer game playerId

continueRoundWithNextPlayer :: Game -> Id -> IO Game 
continueRoundWithNextPlayer game id = do 
  putStrLn "Sorry, you went over 21"
  checkGameStatus (dropPlayer game id) (id + 1)

dropPlayer :: Game -> Id -> Game 
dropPlayer (Game deck players) id =
  Game deck remainingPlayers
  where remainingPlayers =
          filter (\(Player i _ _) -> i /= id) players

continueRoundWithCurrentPlayer :: Game -> Id -> IO Game 
continueRoundWithCurrentPlayer game@(Game _ players) playerId = do
  putStrLn "What would you like to do --"
  putStrLn "(H)it or (S)tand?"
  playerAction <- getLine
  case playerAction of
    "H" -> beginRound (scoreAllPlayers $ dealCard game playerId) playerId
    "S" -> checkGameStatus game (playerId + 1)
    _   -> beginRound game playerId

checkGameStatus :: Game -> Id -> IO Game
checkGameStatus game@(Game deck players) playerId =
  if playerId > (toInteger $ length players) 
    then endRound game
    else beginRound game playerId

endRound :: Game -> IO Game
endRound game = undefined

determineWinner :: Game -> (Id, Score)
determineWinner (Game _ ((Player i _ s):players)) =
  foldr go (i, s) players
  where go (Player id _ score) best =
          if snd best >= score 
            then best 
            else (id, score)

announceWinner :: (Id, Score) -> String
announceWinner winner =
  "The winner is Player " ++ (winnerId) ++ " with a score of " ++ (winnerScore) ++ " points!"
  where winnerId = show $ fst winner
        winnerScore = show $ snd winner