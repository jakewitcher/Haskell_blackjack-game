module GameLogic (setupGame
                 , showPlayerCards
                 , showPlayerScore
                 , checkPlayerScore
                 , scoreAllPlayers
                 , dealCard
                 , checkGameStatus
                 , dropPlayer
                 , determineWinner) where

import Data.List (transpose,  intercalate)
import System.Random (randomRIO)
import GameTypes

suits = [Spade, Club, Heart, Diamond]
ranks = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]

makeDeck :: [Suit] -> [Rank] -> Deck
makeDeck suits ranks =
  Deck $ foldr (\suit cards -> 
                  map (\rank -> go suit rank) ranks ++ cards) [] suits
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

createPlayers :: Integer -> [Maybe Player]
createPlayers 0 = []
createPlayers playerCount =
  createPlayers (playerCount - 1) ++ [Just $ Player playerCount (Hand []) 0]

dealCard :: Game -> Id -> Game
dealCard (Game (Deck deck) players) id = 
  Game d (go id $ players)
  where d = Deck $ drop 1 deck
        c = take 1 deck
        go i =
          map (\m ->deal m i)
        deal m i =
          case m of 
            Just p@(Player x (Hand h) s) ->
              if x == i 
                then Just $ Player x (Hand $ c ++ h) s 
                else Just p
            Nothing -> Nothing

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
  Game deck (map (fmap scorePlayer) players)

setupGame' :: Deck -> Integer -> IO Game
setupGame' deck playerCount = do
  d <- (shuffleDeck deck)
  return $ (scoreAllPlayers . firstHand) (Game d ps)
  where ps = createPlayers playerCount

setupGame :: Integer -> IO Game
setupGame = setupGame' deck

showPlayerCards :: Game -> Id -> String
showPlayerCards (Game _ players) id =
   printPlayerCards players
  where printPlayerCards =
          foldr (\m result ->
                  case m of 
                    Just (Player i (Hand cs) _) -> 
                      if i == id
                        then (++ result) $ (intercalate ", ") . showCards $ cs
                        else "" ++ result
                    Nothing -> "" ++ result
                ) ""
        showCards =
          foldr (\(Card suit rank) s -> (:s) $ (show rank) ++ " of " ++ (show suit) ++ "s") []

showPlayerScore :: Game -> Id -> String
showPlayerScore (Game _ players) id =
  printPlayerScore players
  where printPlayerScore = 
          foldr (\m result -> 
                  case m of
                    Just (Player i _ s) -> 
                      if i == id 
                        then (show s) ++ result 
                        else "" ++ result
                    Nothing -> "" ++ result
                ) ""

getPlayerScore :: Game -> Id -> Maybe Score 
getPlayerScore (Game _ players) id =
  findPlayer id players
  where findPlayer id =
          foldr (\m r -> 
                  case m of 
                    Just (Player i _ s) -> 
                      if i == id 
                        then Just s
                        else r
                    Nothing -> r
                  ) Nothing

checkPlayerScore :: Game -> Id -> Bool 
checkPlayerScore game id =
  let m = getPlayerScore game id
    in case m of
        Just x -> if x > 21 then True else False
        Nothing -> False

dropPlayer :: Game -> Id -> Game 
dropPlayer (Game deck players) id =
  Game deck remainingPlayers
  where remainingPlayers =
          map (\m -> 
                case m of 
                  Just p@(Player i _ _) -> 
                    if i == id 
                      then Nothing 
                      else Just p
                  Nothing -> Nothing
              ) players

checkGameStatus :: Game -> Id -> Bool
checkGameStatus game@(Game deck players) playerId =
  playerId < (toInteger $ length players) 

determineWinner :: Game -> Maybe (Id, Score)
determineWinner (Game _ (player:players)) =
  foldr go Nothing players
  where go m best =
          case m of
            Just (Player id _ score) ->
              case best of
                Just (i, s) -> 
                  if s >= score 
                    then best 
                    else Just (id, score)
                Nothing -> Just (id, score)
            Nothing -> best
