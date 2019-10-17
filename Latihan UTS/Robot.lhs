This code was automatically extracted from a .lhs file that
uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

> module Robot where
>
> import Data.Array
> import Data.List
> import Control.Monad
> import Control.Applicative
> import System.IO
> import SOE

-- import qualified GraphicsWindows as GW (getEvent)

> drawSquare = do 
>      penDown
>      moven 5
>      turnRight
>      moven 5
>      turnRight
>      moven 5
>      turnRight
>      moven 5

< cond :: Robot Bool -> Robot a -> Robot a -> Robot a

< evade :: Robot ()
< evade = cond blocked
<           (do turnRight
<               move)
<           move

> evade :: Robot ()
> evade = do cond1 blocked turnRight 
>            move

> moveToWall :: Robot ()
> moveToWall = while (isnt blocked)
>                move

> getCoinsToWall :: Robot ()
> getCoinsToWall = while (isnt blocked) $
>                    do move
>                       pickCoin

> moven :: Int -> Robot ()
> moven n = mapM_ (const move) [1..n]

> data RobotState 
>   = RobotState 
>         { position  :: Position
>         , facing    :: Direction
>         , pen       :: Bool 
>         , color     :: Color
>         , treasure  :: [Position]
>         , pocket    :: Int
>         }
>      deriving Show


> s0 :: RobotState
> s0 = RobotState { position = (0,0)
>                 , pen      = True
>                 , color    = Cyan
>                 , facing   = North
>                 , treasure = tr
>                 , pocket   = 0
>                 }

> type Position = (Int,Int)

> data Direction = North | East | South | West
>      deriving (Eq,Show,Enum)

< type Robot1 a = RobotState -> Grid -> (RobotState, a)

< type Robot2 a = RobotState -> Grid -> Window -> (RobotState, a, IO ())

< type Robot3 a = RobotState -> Grid -> Window -> IO (RobotState, a)

> newtype Robot a 
>   = Robot (RobotState -> Grid -> Window -> IO (RobotState, a))

> instance Functor Robot where
>    fmap = liftM

> instance Applicative Robot where
>    pure  = return
>    (<*>) = ap 

> instance Monad Robot where
>   return a 
>     = Robot (\s _ _ -> return (s,a))
>   Robot sf0 >>= f
>     = Robot $ \s0 g w -> do
>                 (s1,a1) <- sf0 s0 g w
>                 let Robot sf1 = f a1
>                 (s2,a2) <- sf1 s1 g w
>                 return (s2,a2)

> right,left :: Direction -> Direction

> right d = toEnum (succ (fromEnum d) `mod` 4)
> left  d = toEnum (pred (fromEnum d) `mod` 4)

> updateState :: (RobotState -> RobotState) -> Robot ()
> updateState u = Robot (\s _ _ -> return (u s, ()))

> queryState  :: (RobotState -> a) -> Robot a
> queryState  q = Robot (\s _ _ -> return (s, q s))