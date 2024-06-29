module Main (main, PingPong, design, initialState) where

-- Import libraries
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

-- Window data
name :: String
name = "The Ping-Pong Game"

width, height, offset, fps :: Int
width = 800
height = 800
offset = 10
fps = 60

background :: Color
background = black

-- Data structure
data PingPong = Game
    {
        ballLoc :: (Float, Float),  -- ball location
        ballVel :: (Float, Float),  -- ball velocity
        player1 :: Float,           -- left bar height
        player2 :: Float            -- right bar height
    }
    deriving Show -- print values

-- Design
design :: PingPong -> Picture
design state = pictures[ball, walls, leftBar $ player1 state, rightBar $ player2 state] -- itens
    where
        -- Ball
        ball = uncurry translate (ballLoc state) $ color ballColor $ circleSolid ballRadius
        ballRadius = 10
        ballColor = red

        -- Bottom and top walls
        wall :: Float -> Picture
        wall wallOffset = translate 0 wallOffset $ color wallColor $ rectangleSolid wallWidth wallHeight
        wallColor = greyN 0.5
        wallWidth = fromIntegral width
        wallHeight = 10
        
        walls = pictures[wall position, wall (-position)]
        position = (fromIntegral height / 2) - (wallHeight / 2)
        
        -- Bars
        leftBar, rightBar :: Float -> Picture
        leftBar x = translate 380 x $ color barColor $ rectangleSolid barWidth barHeight
        rightBar y = translate (-380) y $ color barColor $ rectangleSolid barWidth barHeight
        barColor = green
        barWidth = 10
        barHeight = 150

-- Initial state
initialState :: PingPong
initialState = Game
    {
        ballLoc = (0, 0),
        ballVel = (60, 15),
        player1 = 0,
        player2 = 0
    }

-- Ball animation
moveBall :: Float -> PingPong -> PingPong
moveBall seconds state = state {ballLoc = (x', y')}
    where
        -- Current location and velocity
        (x, y) = ballLoc state
        (vx, vy) = ballVel state

        -- New location and velocity
        x' = x + vx * seconds
        y' = y + vy * seconds

-- Define window
window :: Display
window = InWindow name (width, height) (offset, offset)

-- Display window
main :: IO ()
main = simulate window background fps initialState design update

-- Update window
update :: ViewPort -> Float -> PingPong -> PingPong
update _ = moveBall