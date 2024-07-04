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
type Player = (Float, String)   -- bar height and movement
type Score = (Int, Int)     -- players score

-- Data structure
data PingPong = Game
    {
        ballLocation :: Location,   -- ball location
        ballVelocity :: Location,   -- ball velocity
        player1 :: Player,          -- left bar
        player2 :: Player,          -- right bar
        score :: Score,
        paused :: Bool
    }
    deriving Show -- print values

-- Design
design :: PingPong -> Picture
design game = pictures[scoreBoard $ score game, ball, walls, leftBar $ player1 game, rightBar $ player2 game] -- itens
    where
        -- Placar
        scoreBoard :: Score -> Picture
        scoreBoard (s1, s2) = translate (-halfWidth) (-halfHeight) $ scale 0.15 0.15 $ color white $ text (show s1 ++  " vs " ++ show s2)
        halfWidth = fromIntegral width * 0.045
        halfHeight = fromIntegral height * 0.49

        -- Ball
        ball = uncurry translate (ballLocation game) $ color ballColor $ circleSolid ballRadius

        -- Bottom and top walls
        wall :: Float -> Picture
        wall wallOffset = translate 0 wallOffset $ color background $ rectangleSolid wallWidth 0
        wallWidth = fromIntegral width

        walls = pictures[wall position, wall (-position)]
        position = (fromIntegral height / 2)
        
        -- Bars
        rightBar, leftBar :: Player -> Picture
        rightBar (y1, _) = translate barPosition y1 $ color barColor $ rectangleSolid barWidth barHeight
        leftBar (y2, _) = translate (-barPosition) y2 $ color barColor $ rectangleSolid barWidth barHeight

-- Initial state
initialState :: PingPong
initialState = Game
    {
        ballLocation = (0, 0),
        ballVelocity = (200, 40),
        player1 = (0, "Still"),
        player2 = (0, "Still"),
        score = (0, 0),
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

-- Bars movement
moveBars :: Float -> PingPong -> PingPong
moveBars seconds game = game {player1 = player1', player2 = player2'}
    where
        move1 = snd $ player1 game
        move2 = snd $ player2 game

        player1' = (movement (player1 game) seconds, move1)
        player2' = (movement (player2 game) seconds, move2)

-- Player movement
movement :: Player -> Float -> Float
movement (y, move) seconds
    | move == "Up" = barsLimit (y + 300 * seconds)
    | move == "Down" = barsLimit (y - 300 * seconds)
    | otherwise = y

barsLimit :: Float -> Float
barsLimit y
    | y >= limit = limit
    | y <= -limit = -limit
    | otherwise = y
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
        y1 = fst $ player1 game
        y2 = fst $ player2 game

        xCollision = abs(x) + ballRadius >= limWidth
        limWidth = barPosition - (barWidth / 2)

        -- xCollision

        rightCollision =  xCollision && playerCollision (y1) y
        leftCollision = xCollision && playerCollision (y2) y

-- Bar limits collision
playerCollision :: Float -> Float -> Bool
playerCollision yn y = upperLimit && lowerLimit
    where
        upperLimit = y - ballRadius < yn + halfbar
        lowerLimit = y + ballRadius > yn - halfbar
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

scoring :: PingPong -> PingPong
scoring game
    | rightPlayer = initialState {score = (s1, s2 + 1)}
    | leftPlayer = initialState {score = (s1 + 1, s2)}
    | otherwise = game
    where
        (s1, s2) = score game
        x = fst $ ballLocation game
        rightPlayer = x + ballRadius >= limWidth
        leftPlayer = x - ballRadius <= -limWidth
        limWidth = barPosition + (barWidth / 2) + 2 * ballRadius

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
    | otherwise = scoring $ bounce $ moveBars seconds $ moveBall seconds game

-- Reset game
handleKeys :: Event -> PingPong -> PingPong
handleKeys (EventKey (SpecialKey KeySpace) _ _ _) game = game { paused = False }
handleKeys (EventKey (Char 'p') _ _ _) game = game { paused = True }
handleKeys (EventKey (Char 'r') _ _ _) _ = initialState

handleKeys (EventKey (Char 'w') Down _ _) game = game { player1 = (fst $ player1 game, "Up")}
handleKeys (EventKey (Char 'w') Up _ _) game = game { player1 = (fst $ player1 game, "Still")}

handleKeys (EventKey (Char 's') Down _ _) game = game { player1 = (fst $ player1 game, "Down")}
handleKeys (EventKey (Char 's') Up _ _) game = game { player1 = (fst $ player1 game, "Still")}

handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = game { player2 = (fst $ player2 game, "Up")}
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game = game { player2 = (fst $ player2 game, "Still")}

handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game = game { player2 = (fst $ player2 game, "Down")}
handleKeys (EventKey (SpecialKey KeyDown) Up _ _) game = game { player2 = (fst $ player2 game, "Still")}
    
handleKeys _ game = game