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