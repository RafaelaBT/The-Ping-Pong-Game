module Main (main) where

-- Import libraries
import Graphics.Gloss

-- Window shape
width, height, offset :: Int
width = 800
height = 500
offset = 10

-- Create window
window :: Display
window = InWindow "The Ping-Pong Game" (width, height) (offset, offset)

-- Background color
background :: Color
background = black

-- Create drawing
drawing :: Picture
drawing = pictures[
    -- Objects
    ball,
    walls,
    rightBar,
    leftBar
    ]
    where
        -- Ball
        ball = translate ballRadius ballRadius $ color red $ circleSolid ballRadius
        ballRadius = 10

        -- Bottom and top walls
        wall :: Float -> Picture
        wall offset = translate 0 offset $ color wallColor $ rectangleSolid wallWidth wallHeight
        wallColor = white
        wallWidth = fromIntegral width
        wallHeight = 10
        
        walls = pictures[wall position, wall (-position)]
        position = (fromIntegral height / 2) - (wallHeight / 2)

        -- Bars
        rightBar = translate 380 (0) $ color blue $ rectangleSolid barWidth barHeight
        leftBar = translate (-380) 0 $ color green $ rectangleSolid barWidth barHeight
        barWidth = 10
        barHeight = 300

-- Display window
main :: IO ()
main = display window background drawing
