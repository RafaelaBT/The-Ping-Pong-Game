module Main (main) where

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
barPosition = 395
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
        player1 :: Float,           -- left bar location
        player2 :: Float,           -- right bar location
        paused :: Bool
    }
    deriving Show -- print values

-- Design
design :: PingPong -> Picture
design game = pictures[ball, walls, leftBar $ player1 game, rightBar $ player2 game] -- itens
    where
        -- Ball
        ball = uncurry translate (ballLocation game) $ color ballColor $ circleSolid ballRadius

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
        ballVelocity = (200, 40),
        player1 = 0,
        player2 = 0,
        paused = True
    }

-- Ball moviment
moveBall :: Float -> PingPong -> PingPong
moveBall seconds game = game {ballLocation = (x', y')}
    where
        -- Current location and velocity
        (x, y) = ballLocation game
        (vx, vy) = ballVelocity game

        -- New location and velocity
        x' = x + vx * seconds
        y' = y + vy * seconds

-- Bars moviment
moveBars :: Float -> PingPong -> PingPong
moveBars seconds game = game {player1 = player1', player2 = player2'}
    where
        player1' = barsLimit (player1 game) + seconds
        player2' = barsLimit (player2 game) + seconds

barMovement :: Float -> String -> Float
barMovement player move
    | move == "Up" = upMovement player
    | otherwise = downMovement player

upMovement :: Float -> Float
upMovement player = player + 10

downMovement :: Float -> Float
downMovement player = player - 10

barsLimit :: Float -> Float
barsLimit player
    | player >= limit = limit
    | player <= -limit = -limit
    | otherwise = player
    where
        limit = (fromIntegral height / 2) - (barHeight / 2)

-- Wall collision (top or bottom)
wallCollision :: Location -> Bool
wallCollision (_, y) = abs(y) + ballRadius >= limHeight
    where
        limHeight = fromIntegral height / 2

-- Bar collision (right or left)
barCollision :: PingPong -> Bool
barCollision game = rightCollision || leftCollision
    where
        (x, y) = ballLocation game

        xCollision = abs(x) + ballRadius >= limWidth
        limWidth = barPosition - (barWidth / 2)

        rightCollision = xCollision && playerCollision (player1 game) y
        leftCollision = xCollision && playerCollision (player2 game) y

-- Bar limits collision
playerCollision :: Float -> Float -> Bool
playerCollision player y = upperLimit && lowerLimit
    where
        upperLimit = y <= player + halfbar
        lowerLimit = y >= player - halfbar
        halfbar = barHeight / 2

-- Wall bounce
bounce :: PingPong -> PingPong
bounce game = game { ballVelocity = (vx', vy')}
    where
        -- Current velocity
        (vx, vy) = ballVelocity game
        wallCol = wallCollision (ballLocation game)
        barCol = barCollision (game)

        -- New velocity
        (vx', vy')
            | wallCol && barCol = (-vx, -vy)
            | wallCol = (vx, -vy)
            | barCol = (-vx, vy)
            | otherwise = (vx, vy)

-- Define window
window :: Display
window = InWindow name (width, height) (offset, offset)

-- Display window
main :: IO ()
main = play window background fps initialState design handleKeys update

-- Update window
update :: Float -> PingPong -> PingPong
update seconds game
    | paused game = game
    | otherwise = bounce $ moveBars seconds $ moveBall seconds game

-- Reset game
handleKeys :: Event -> PingPong -> PingPong
handleKeys (EventKey (SpecialKey KeySpace) _ _ _) game = game { paused = False }
handleKeys (EventKey (Char 'p') _ _ _) game = game { paused = True }
handleKeys (EventKey (Char 'r') _ _ _) _ = initialState

handleKeys (EventKey (Char 'w') _ _ _) game = game { player1 = barMovement (player1 game) "Up"}
handleKeys (EventKey (Char 's') _ _ _) game = game { player1 = barMovement (player1 game) "Down"}
handleKeys (EventKey (SpecialKey KeyUp) _ _ _) game = game { player2 = barMovement (player2 game) "Up"}
handleKeys (EventKey (SpecialKey KeyDown) _ _ _) game = game { player2 = barMovement (player2 game) "Down"}

handleKeys _ game = game