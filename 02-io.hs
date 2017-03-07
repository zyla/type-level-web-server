
import Prelude hiding (IO, String)
import GHC.TypeLits

type String = [Nat]

data IO a =
    Return a
  | PutStr String (IO a)
  | GetLine (String -> IO a)

type HelloWorld =
  -- "Hello World!\n"
  [72,101,108,108,111,32,87,111,114,108,100,33,10]

type Main = PutStr HelloWorld
          $ GetLine Main1

type family Main1 (input :: String) :: IO () where
  Main1 input = PutStr (input ++ '[10])
              $ Return '()

type f $ x = f x

type family (xs :: [a]) ++ (ys :: [a]) :: [a] where
  '[] ++ ys       = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)
