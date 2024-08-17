module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Game
import Rendering
import Input
import Variables

-- Define window and run game
main :: IO ()
main = do
    (screenWidth, screenHeight) <- getScreenSize
    let halfWidth = screenWidth `div` 2
        halfHeight = screenHeight `div` 2
        barWidth = fromIntegral halfWidth * 0.025    -- 5% da largura da metade da tela
        barHeight = fromIntegral halfHeight * 0.2   -- 30% da altura da metade da tela
        window = InWindow name (halfWidth, halfHeight) (offset, offset)
    play window black fps (initialState barWidth barHeight) (design halfWidth halfHeight barWidth barHeight) handleKeys (update halfWidth halfHeight barWidth barHeight)

-- Update window
update :: Int -> Int -> Float -> Float -> Float -> PingPong -> PingPong
update halfWidth halfHeight barWidth barHeight seconds game
    | paused game = game
    | otherwise = scoring halfWidth halfHeight barWidth barHeight
                  $ ballPaddleBounce
                  $ movePaddles
                  $ moveBall game 
    where
        moveBall g = g { gameBall = move (SimpleMove seconds) (gameBall g) }
        movePaddles g = g { paddles = move (ComplexMove halfHeight barWidth barHeight seconds) (paddles g) }
        ballPaddleBounce g = g {gameBall = bounce (BallPaddle halfWidth halfHeight barWidth barHeight (paddles g)) (gameBall g)}

 