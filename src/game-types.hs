module GameTypes where

data Suit = Spade
          | Club
          | Heart
          | Diamond
          deriving Show

data Card = Face Suit Char 
          | Rank Suit Int
          deriving Show

data Deck = Deck [Card]
          deriving Show

data Hand = Hand [Card]

type Score = Int

data Player = Player Hand Score

data Game = Game Deck [Player]