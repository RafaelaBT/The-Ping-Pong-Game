module Game
    ( PingPong(..)
    , initialState
    , moveBall
    , moveBars
    , barCollision
    , wallCollision
    , bounce
    , scoring
    ) where

import Graphics.Gloss
import Variables

-- Data structure
data PingPong = Game
    { ballLocation  :: Location
    , ballVelocity  :: Location
    , player1       :: Player
    , player2       :: Player
    , score         :: Score
    , paused        :: Bool
    , gameBarWidth  :: Float  -- Renomeado para evitar ambiguidade
    , gameBarHeight :: Float  -- Renomeado para evitar ambiguidade
    } deriving Show

-- Initial state
initialState :: Float -> Float -> PingPong
initialState barWidth barHeight = Game
    { ballLocation  = (0, 0)
    , ballVelocity  = (200, 40)
    , player1       = (0, "Still")
    , player2       = (0, "Still")
    , score         = (0, 0)
    , paused        = True
    , gameBarWidth  = barWidth
    , gameBarHeight = barHeight
    }

-- Ball movement
moveBall :: Float -> PingPong -> PingPong
moveBall seconds game = game { ballLocation = (x', y') }
    where
        (x, y) = ballLocation game
        (vx, vy) = ballVelocity game
        x' = x + vx * seconds
        y' = y + vy * seconds

-- Bars movement
moveBars :: Int -> Float -> Float -> Float -> PingPong -> PingPong
moveBars screenHeight barWidth barHeight seconds game = game { player1 = player1', player2 = player2' }
    where
        move1 = snd $ player1 game
        move2 = snd $ player2 game

        player1' = (movement screenHeight barHeight (player1 game) seconds, move1)
        player2' = (movement screenHeight barHeight (player2 game) seconds, move2)

-- Player movement
movement :: Int -> Float -> Player -> Float -> Float
movement screenHeight barHeight (y, move) seconds
    | move == "Up"   = barsLimit screenHeight barHeight (y + 300 * seconds)
    | move == "Down" = barsLimit screenHeight barHeight (y - 300 * seconds)
    | otherwise      = y

barsLimit :: Int -> Float -> Float -> Float
barsLimit screenHeight barHeight y
    | y >= limit = limit
    | y <= -limit = -limit
    | otherwise = y
    where
        limit = (fromIntegral screenHeight / 2) - (barHeight / 2)

-- Wall collision (top or bottom)
wallCollision :: Int -> Location -> Bool
wallCollision screenHeight (_, y) = abs y + ballRadius >= limHeight
    where
        limHeight = fromIntegral screenHeight / 2

-- Bar collision (right or left)
barCollision :: Int -> Float -> Float -> PingPong -> Bool
barCollision screenWidth barWidth barHeight game
    | x >= 0    = rightCollision
    | otherwise = leftCollision
    where
        (x, y) = ballLocation game
        y1 = fst $ player1 game
        y2 = fst $ player2 game

        xCollision = abs x + ballRadius >= limWidth
        limWidth = screenBarPosition screenWidth - (barWidth / 2)

        leftCollision = xCollision && playerCollision y1 y barHeight
        rightCollision = xCollision && playerCollision y2 y barHeight

-- Bar limits collision
playerCollision :: Float -> Float -> Float -> Bool
playerCollision yn y barHeight = upperLimit && lowerLimit
    where
        upperLimit = y - ballRadius < yn + halfbar
        lowerLimit = y + ballRadius > yn - halfbar
        halfbar = barHeight / 2

-- Wall bounce
bounce :: Int -> Int -> Float -> Float -> PingPong -> PingPong
bounce screenWidth screenHeight barWidth barHeight game = game { ballVelocity = (vx', vy') }
    where
        (vx, vy) = ballVelocity game
        wallCol = wallCollision screenHeight (ballLocation game)
        barCol = barCollision screenWidth barWidth barHeight game

        (vx', vy')
            | wallCol && barCol = (-vx, -vy)
            | wallCol           = (vx, -vy)
            | barCol            = (-vx, vy)
            | otherwise         = (vx, vy)

scoring :: Int -> Int -> Float -> Float -> PingPong -> PingPong
scoring screenWidth screenHeight barWidth barHeight game
    | leftPlayer  = (initialState barWidth barHeight) { score = (s1, s2 + 1) }
    | rightPlayer = (initialState barWidth barHeight) { score = (s1 + 1, s2) }
    | otherwise   = game
    where
        (s1, s2) = score game
        x = fst $ ballLocation game
        rightPlayer = x + ballRadius >= limWidth
        leftPlayer  = x - ballRadius <= -limWidth
        limWidth = screenBarPosition screenWidth + (barWidth / 2) + 2 * ballRadius
