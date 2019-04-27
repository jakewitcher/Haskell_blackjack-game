module GameTypes where

data Suit = Spade
          | Club
          | Heart
          | Diamond
          deriving Show

data Rank = Ace
          | Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          deriving Show

data Card = Card Suit Rank
          deriving Show

data Deck = Deck [Card]
          deriving Show

data Hand = Hand [Card]

type Score = Integer

data Player = Player Hand Score

data Game = Game Deck [Player]