module Input
    ( handleKeys
    ) where

import Graphics.Gloss.Interface.Pure.Game
import Game

-- Handle keys
handleKeys :: Event -> PingPong -> PingPong
handleKeys (EventKey (Char 'r') _ _ _) game = initialState (paddleWidth $ paddles game) (paddleHeight $ paddles game)
handleKeys (EventKey (SpecialKey KeySpace) _ _ _) game = game { paused = False }
handleKeys (EventKey (Char 'p') _ _ _) game = game { paused = True }

handleKeys (EventKey (Char 'w') Down _ _) game = game { paddles =  (paddles game) {p1 = (fst $ p1 $ paddles game, "Up")}}
handleKeys (EventKey (Char 'w') Up _ _) game = game { paddles = (paddles game) {p1 = (fst $ p1 $ paddles game, "Still")}}

handleKeys (EventKey (Char 's') Down _ _) game = game { paddles = (paddles game) {p1 = (fst $ p1 $ paddles game, "Down")}}
handleKeys (EventKey (Char 's') Up _ _) game = game { paddles = (paddles game) {p1 = (fst $ p1 $ paddles game, "Still")}}

handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = game { paddles = (paddles game) {p2 = (fst $ p2 $ paddles game, "Up")}}
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game = game { paddles = (paddles game) {p2 = (fst $ p2 $ paddles game, "Still")}}

handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game = game { paddles = (paddles game) {p2 = (fst $ p2 $ paddles game, "Down")}}
handleKeys (EventKey (SpecialKey KeyDown) Up _ _) game = game { paddles = (paddles game) {p2 = (fst $ p2 $ paddles game, "Still")}}

handleKeys _ game = game

