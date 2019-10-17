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

