module Game
    ( PingPong(..)
    , initialState
    , bounce
    , scoring
    , MoveParams(..)
    , Movable(..)
    , Ball(..)
    , Paddles(..)
    , BounceParams(..)
    , Bouncer(..)
    ) where


import Graphics.Gloss
import Variables


data PingPong = Game
    { gameBall      :: Ball
    , paddles       :: Paddles -- player1, player2, gameBarWidth and gameBarHeight
    , score         :: Score
    , paused        :: Bool
    } deriving Show



-- Initial state 2
initialState :: Float -> Float -> PingPong
initialState barWidth barHeight = Game
    { gameBall         =  Ball (0,0) (200,40) ballRadius
    , paddles       = Paddles barWidth barHeight (0, "Still") (0, "Still")
    , score         = (0, 0)
    , paused        = True
    }

    


-- Class Type Generalization

data MoveParams
    = SimpleMove Float  -- Move with just a time value
    | ComplexMove Int Float Float Float  -- Move with additional parameters

class Movable a where
    move :: MoveParams -> a -> a

data Ball = Ball
    { ballLocation2 :: Location
    , ballVelocity2 :: Location
    , radius        :: Float
    } deriving (Show, Eq)

instance Movable Ball where
    move (SimpleMove seconds) ball = ball { ballLocation2 = (x', y') }
        where
            (x, y) = ballLocation2 ball
            (vx, vy) = ballVelocity2 ball
            x' = x + vx * seconds
            y' = y + vy * seconds

data Paddles = Paddles
    { paddleWidth  :: Float
    , paddleHeight :: Float
    , p1           :: Player
    , p2           :: Player 
    } deriving Show

instance Movable Paddles where
    move (ComplexMove screenHeight barWidth barHeight seconds) paddle = paddle {p1 = player1', p2 = player2'}
        where
            move1 = snd $ p1 paddle
            move2 = snd $ p2 paddle


            player1' = (movement screenHeight barHeight (p1 paddle) seconds, move1)
            player2' = (movement screenHeight barHeight (p2 paddle) seconds, move2)


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


data ColideParams
    = WallCollide Int  
    | PaddleCollide Int Float Float Paddles
    deriving Show

class Colliders a where
    collide :: ColideParams -> a -> Bool



instance Colliders Ball where
  -- Wall collision (top or bottom)
    collide (WallCollide screenHeight) ball = abs y + (radius ball) >= limHeight
        where
            limHeight = fromIntegral screenHeight / 2  
            y = snd $ ballLocation2 ball

    -- PaddleCollision
    collide (PaddleCollide screenWidth barWidth barHeight paddle) ball 
        | x >= 0    = rightCollision
        | otherwise = leftCollision
        where
            (x, y) = ballLocation2 $ ball
            y2 = fst $ p2 $ paddle
            y1 = fst $ p1 $ paddle

            xCollision = abs x + (radius ball) >= limWidth
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

data BounceParams
    = BallPaddle Int Int Float Float Paddles
    | Another Int Float Float Paddles
    deriving Show

class Bouncer a where
    bounce :: BounceParams -> a -> Ball

instance Bouncer Ball where
    bounce (BallPaddle screenWidth screenHeight barWidth barHeight paddle) ball = ball {ballVelocity2 = (vx', vy') }
        where
            (vx, vy) = ballVelocity2 $ ball
            wallCol = collide (WallCollide screenHeight) $ ball
            

            barCol = collide (PaddleCollide screenWidth barWidth barHeight (paddle)) $ ball
            

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
        x = fst $ ballLocation2 $ gameBall game 
        rightPlayer = x + ballRadius >= limWidth
        leftPlayer  = x - ballRadius <= -limWidth
        limWidth = screenBarPosition screenWidth + (barWidth / 2) + 2 * ballRadius
