module Rendering
    ( design
    ) where

import Graphics.Gloss
import Game
import Variables

-- Design
design :: Int -> Int -> Float -> Float -> PingPong -> Picture
design screenWidth screenHeight barWidth barHeight game = pictures [scoreBoard $ score game, ball, walls, leftBar $ player1 game, rightBar $ player2 game]
    where
        -- Scoreboard
        scoreBoard :: Score -> Picture
        scoreBoard (s1, s2) = translate (-halfWidth) (-halfHeight) $ scale 0.15 0.15 $ color white $ text (show s1 ++  " vs " ++ show s2)
        halfWidth = fromIntegral screenWidth * 0.045
        halfHeight = fromIntegral screenHeight * 0.49

        -- Ball
        ball = uncurry translate (ballLocation game) $ color ballColor $ circleSolid ballRadius

        -- Bottom and top walls
        wall :: Float -> Picture
        wall wallOffset = translate 0 wallOffset $ color background $ rectangleSolid wallWidth 0
        wallWidth = fromIntegral screenWidth

        walls = pictures [wall position, wall (-position)]
        position = fromIntegral screenHeight / 2

        -- Bars
        rightBar, leftBar :: Player -> Picture
        rightBar (y1, _) = translate (screenBarPosition screenWidth) y1 $ color barColor $ rectangleSolid barWidth barHeight
        leftBar (y2, _) = translate (-screenBarPosition screenWidth) y2 $ color barColor $ rectangleSolid barWidth barHeight
