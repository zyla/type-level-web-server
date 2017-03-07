{-# LANGUAGE DataKinds, TypeFamilies,
   TypeOperators, TypeInType, UndecidableInstances #-}
module Example1 where

import Prelude hiding (IO, String)
import GHC.TypeLits

type String = [Nat]

data IO a =
    Return a
  | PutStr String (IO a)
  | GetLine Cont

-- defunctionalization

type family Apply
              (k :: Cont)
              (line :: String)
              :: IO ()

-- String -> IO a
data Cont =
    Main1 -- process the first line
  | Main2 String -- process the second line

type HelloWorld =
  -- "Hello World!\n"
  [72,101,108,108,111,32,87,111,114,108,100,33,10]

-- Write "Hello world", read two lines,
-- contatenate them and output them
type Main = PutStr HelloWorld
          $ GetLine Main1

type instance Apply
    Main1 line1 = GetLine (Main2 line1)

type instance Apply
    (Main2 line1) line2
      = PutStr (line1 ++ line2 ++ '[10])
      $ Return '()

-- input:
-- BC     [66,67]
--
-- program state:
-- Apply (Main2 [65,65]) [66,67]
--
-- output:
-- Hello World!
-- AABC






























type f $ x = f x

type family (xs :: [a]) ++ (ys :: [a]) :: [a] where
  '[] ++ ys       = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- hack
type family Force x where
  Force (Return x) = Return x
  Force (PutStr str k) = PutStr str k
  Force x = x
