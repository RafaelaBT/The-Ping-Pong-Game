module Main (main, PingPong, design, initialState) where

-- Import libraries
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Game properties
name :: String
name = "The Ping-Pong Game"

width, height, offset, fps :: Int
width = 800
height = 800
offset = 10
fps = 60

barWidth, barHeight, barPosition, ballRadius :: Float
barWidth = 10
barHeight = 150
barPosition = 380
ballRadius = 10

background,barColor, ballColor:: Color
background = black
barColor = green
ballColor = red

-- Location data type
type Location = (Float, Float)

-- Data structure
data PingPong = Game
    {
        ballLocation :: Location,   -- ball location
        ballVelocity :: Location,   -- ball velocity
        player1 :: Float,           -- left bar height
        player2 :: Float            -- right bar height
    }
    deriving Show -- print values

-- Design
design :: PingPong -> Picture
design state = pictures[ball, walls, rightBar $ player1 state, leftBar $ player2 state] -- itens
    where
        -- Ball
        ball = uncurry translate (ballLocation state) $ color ballColor $ circleSolid ballRadius

        -- Bottom and top walls
        wall :: Float -> Picture
        wall wallOffset = translate 0 wallOffset $ color background $ rectangleSolid wallWidth 0
        wallWidth = fromIntegral width

        walls = pictures[wall position, wall (-position)]
        position = (fromIntegral height / 2)
        
        -- Bars
        rightBar, leftBar :: Float -> Picture
        rightBar y1 = translate barPosition y1 $ color barColor $ rectangleSolid barWidth barHeight
        leftBar y2 = translate (-barPosition) y2 $ color barColor $ rectangleSolid barWidth barHeight

-- Initial state
initialState :: PingPong
initialState = Game
    {
        ballLocation = (0, 0),
        ballVelocity = (50, 50),
        player1 = 0,
        player2 = 0
    }

-- Ball animation
moveBall :: Float -> PingPong -> PingPong
moveBall seconds state = state {ballLocation = (x', y')}
    where
        -- Current location and velocity
        (x, y) = ballLocation state
        (vx, vy) = ballVelocity state

        -- New location and velocity
        x' = x + vx * seconds
        y' = y + vy * seconds

-- Wall collision (top or bottom)
wallCollision :: Location -> Bool
wallCollision (_, y) = topCollision || bottomCollision
    where
        -- Is there a y collision?
        topCollision = y + ballRadius >= limHeight
        bottomCollision = -y + ballRadius >= limHeight
        limHeight = fromIntegral height / 2

-- Bar collision (right or left)
barCollision :: Location -> Bool
barCollision (x, _) = rightCollision || leftCollision
    where
        -- Is there a x collision?
        rightCollision = x + ballRadius >= limWidth
        leftCollision = -x + ballRadius >= limWidth
        limWidth = barPosition - (barWidth / 2)

-- Wall bounce
bounce :: PingPong -> PingPong
bounce state = state { ballVelocity = (vx', vy')}
    where
        -- Current velocity
        (vx, vy) = ballVelocity state

        -- New velocity
        (vx', vy')
            | wallCollision (ballLocation state) = (vx, -vy * 1.5)
            | barCollision (ballLocation state) = (-vx * 1.5, vy)
            | otherwise = (vx, vy)

-- Define window
window :: Display
window = InWindow name (width, height) (offset, offset)

-- Display window
main :: IO ()
main = play window background fps initialState design handleKeys update

-- Update window
update :: Float -> PingPong -> PingPong
update seconds = bounce . moveBall seconds

-- Reset game
handleKeys :: Event -> PingPong -> PingPong
handleKeys (EventKey (Char 'r') _ _ _) state = state { ballLocation = (0, 0) }
handleKeys _ state = state