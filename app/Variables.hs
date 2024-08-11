module Variables 
    (name
    , offset
    , fps
    , barWidth
    , barHeight
    , ballRadius
    , background
    , barColor
    , ballColor
    , Location
    , Player
    , Score 
    , screenBarPosition) where

import Graphics.Gloss

-- Game properties
name :: String
name = "The Ping-Pong Game"

offset, fps :: Int
offset = 10
fps = 60

barWidth, barHeight, ballRadius :: Float
barWidth = 10
barHeight = 150
ballRadius = 10

background, barColor, ballColor :: Color
background = black
barColor = green
ballColor = red

-- Location data type
type Location = (Float, Float)
type Player = (Float, String)   -- bar height and movement
type Score = (Int, Int)     -- players score

screenBarPosition :: Int -> Float
screenBarPosition screenWidth = fromIntegral screenWidth / 2 - barWidth / 2
