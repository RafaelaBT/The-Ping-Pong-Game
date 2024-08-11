module Input
    ( handleKeys
    ) where

import Graphics.Gloss.Interface.Pure.Game
import Game

-- Handle keys
handleKeys :: Event -> PingPong -> PingPong
handleKeys (EventKey (Char 'r') _ _ _) game = initialState (gameBarWidth game) (gameBarHeight game)
handleKeys (EventKey (SpecialKey KeySpace) _ _ _) game = game { paused = False }
handleKeys (EventKey (Char 'p') _ _ _) game = game { paused = True }

handleKeys (EventKey (Char 'w') Down _ _) game = game { player1 = (fst $ player1 game, "Up")}
handleKeys (EventKey (Char 'w') Up _ _) game = game { player1 = (fst $ player1 game, "Still")}

handleKeys (EventKey (Char 's') Down _ _) game = game { player1 = (fst $ player1 game, "Down")}
handleKeys (EventKey (Char 's') Up _ _) game = game { player1 = (fst $ player1 game, "Still")}

handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = game { player2 = (fst $ player2 game, "Up")}
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game = game { player2 = (fst $ player2 game, "Still")}

handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game = game { player2 = (fst $ player2 game, "Down")}
handleKeys (EventKey (SpecialKey KeyDown) Up _ _) game = game { player2 = (fst $ player2 game, "Still")}

handleKeys _ game = game

