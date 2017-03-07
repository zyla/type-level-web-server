{-# LANGUAGE DataKinds, TypeOperators, TypeInType, TypeFamilies, UndecidableInstances #-}
module HttpServer where

import Prelude hiding (Read, IO)
import GHC.Types (Type)
import GHC.TypeLits (Nat)

import TypoParsec (Apply, Satisfy, Many, Fmap, Ap, WhiteSpace, RunParser, MkFun, CharP)
import qualified TypoParsec

-- the IO library
type Bytes = [Nat]

type Port = Nat
type Fd = Nat

data IO a =
    Exit
  | Return a
  | Listen Port (Fd ~> IO a)
  | Accept Fd (Fd ~> IO a)
  | Write Fd Bytes (IO a)
  | Read Fd Nat (Bytes ~> IO a)
  | Open Bytes (Maybe Fd ~> IO a)
  | Close Fd (IO a)

type BUFSIZE = 4096

--   LABEL                    ACTION                     TARGET

---- Main
-------------------------------------------------------------------------------
type Main                   = Listen 3000              $ AcceptLoop
type instance
     AcceptLoop $$ serverFd = Accept serverFd Return_ >>=
                              HandleClient >>=          (AcceptLoop1 serverFd)
type instance
     AcceptLoop1 serverFd
              $$ '()        =                            AcceptLoop $$ serverFd

data AcceptLoop :: Fd ~> IO ()
data AcceptLoop1 serverFd :: () ~> IO ()

---- HandleClient
-------------------------------------------------------------------------------
data HandleClient :: Fd ~> IO ()
type instance
     HandleClient
        $$ clientFd         = Read clientFd BUFSIZE    $ HandleClient1 clientFd
data HandleClient1 clientFd :: Bytes ~> IO ()
type instance
     HandleClient1 clientFd
        $$ requestBytes     = HandleParsedRequest clientFd (ParseRequest requestBytes) >>=
                              HandleClient2 clientFd
data HandleClient2 clientFd :: () ~> IO ()
type instance
     HandleClient2 clientFd
        $$ '()              = Close clientFd
                            $ Return '()

---- HandleParsedRequest
-------------------------------------------------------------------------------
type family HandleParsedRequest (clientFd :: Fd) (mreq :: Maybe Request) :: IO () where
    --                                             '/'
    HandleParsedRequest clientFd (Just (Req method (47 : filename) httpVer headers)) =
        Open filename $ ServeFile filename clientFd

    HandleParsedRequest clientFd req =
        Write clientFd
          -- "HTTP/1.1 400 Bad Request\r\n\r\n"
          [72,84,84,80,47,49,46,49,32,52,48,48,32,66,97,100,32,82,101,113,117,101,115,116,13,10,13,10]
      $ Return '()


---- ServeFile
-------------------------------------------------------------------------------
data ServeFile (filename :: Bytes) (clientFd :: Fd) :: Maybe Fd ~> IO ()
type instance
     ServeFile filename clientFd
            $$ Just fileFd  = Write clientFd
                                -- "HTTP/1.1 200 OK\r\n\r\n"
                                [72,84,84,80,47,49,46,49,32,50,48,48,32,79,75,13,10,13,10]
                            $ CopyData fileFd clientFd
type instance
     ServeFile filename clientFd
            $$ Nothing      = Write clientFd
                                -- "HTTP/1.1 404 Not Found ..."
                                ([72,84,84,80,47,49,46,49,32,52,48,52,32,78,111,116,32,70,111,117,110,100,13,10,67,111,110,116,101,110,116,45,116,121,112,101,58,32,116,101,120,116,47,104,116,109,108,13,10,13,10,60,104,49,62,78,111,116,32,102,111,117,110,100,58,32]
                                ++ filename
                                ++ [60,47,104,49,62])
                            $ Return '()

---- CopyData
-------------------------------------------------------------------------------
type CopyData
       fileFd clientFd      = Read fileFd BUFSIZE     $ CopyData1 fileFd clientFd

data CopyData1 fileFd clientFd :: Bytes ~> IO ()
type instance
     CopyData1
       fileFd clientFd
       $$ '[]               = Close fileFd
                            $ Return '()
type instance
     CopyData1
       fileFd clientFd
       $$ (x ': xs)         = Write clientFd (x ': xs)
                            $ CopyData fileFd clientFd
      

---- ParseRequest
-------------------------------------------------------------------------------
type family ParseRequest (reqBytes :: Bytes) :: Maybe Request where
    ParseRequest reqBytes = GetResult (RunParser RequestP reqBytes)

type family GetResult (parseResult :: Maybe (Request, Bytes)) :: Maybe Request where
    GetResult Nothing = Nothing
    GetResult (Just '(result, bytes)) = Just result

data Request = Req Bytes Bytes Bytes [Header]
data Header = Hdr Bytes Bytes

type a *> b = a TypoParsec.*> b
type a <* b = a TypoParsec.<* b













-- "GET /alha HTTP/1.1"
type RequestP =
   Req
     `Fmap` Token `Ap` Token `Ap` Token
     `Ap`   (WhiteSpace *> Many HeaderP)

type Token = WhiteSpace *> Many (Satisfy NotSpace)

type COLON = 58

type HeaderP = Hdr
  `Fmap` Many (Satisfy NotColon)
  `Ap` (CharP COLON *> Many (Satisfy NotNewline) <* WhiteSpace)













headerPTest :: (
                      -- "ab:cd\r\n"
   RunParser HeaderP '[99,98,58,99,100,13,10]
 ~ Just '( Hdr '[99,98] '[99,100], '[] )
 ) => ()
headerPTest = ()

requestPTest1 = () :: (
    RunParser RequestP
        -- "GET /index.html HTTP/1.1\r\n\r\n"
        '[71,69,84,32,47,105,110,100,101,120,46,104,116,109,108,32,72,84,84,80,47,49,46,49,13,10,13,10]
                 -- "GET"
  ~ Just '( Req '[71,69,84]
                 -- "/index.html"
                '[47,105,110,100,101,120,46,104,116,109,108]
                 -- "HTTP/1.1"
                '[72,84,84,80,47,49,46,49]
                '[], -- headers
            '[] ) ) => ()

type NotSpace = MkFun Nat Bool NotSpace_
data NotSpace_
type instance Apply (TypoParsec.F NotSpace_) (c :: Nat) = IsNotSpace c

type family IsNotSpace (c :: Nat) :: Bool where
    IsNotSpace 32 = False
    IsNotSpace 10 = False
    IsNotSpace 13 = False
    IsNotSpace c  = True

type NotNewline = MkFun Nat Bool NotNewline_
data NotNewline_
type instance Apply (TypoParsec.F NotNewline_) (c :: Nat) = IsNotNewline c

type family IsNotNewline (c :: Nat) :: Bool where
    IsNotNewline 10 = False
    IsNotNewline 13 = False
    IsNotNewline c  = True

type NotColon = MkFun Nat Bool NotColon_
data NotColon_
type instance Apply (TypoParsec.F NotColon_) (c :: Nat) = IsNotColon c

type family IsNotColon (c :: Nat) :: Bool where
    IsNotColon 58 = False
    IsNotColon c  = True

data Return_ :: a ~> IO a
type instance Return_ $$ x = Return x

--

type family (ma :: IO a) >>= (k :: a ~> IO b) :: IO b where
  Exit              >>= k = Exit
  Return a          >>= k = k $$ a
  Listen port k1    >>= k = Listen port    (k1 >=> k)
  Accept fd   k1    >>= k = Accept fd      (k1 >=> k)
  Write fd bytes k1 >>= k = Write fd bytes (k1 >>= k)
  Read fd len k1    >>= k = Read fd len    (k1 >=> k)
  Open filename k1  >>= k = Open filename  (k1 >=> k)
  Close fd k1       >>= k = Close fd       (k1 >>= k)

type family (k1 :: (a :: Type) ~> IO (b :: Type)) >=> (k2 :: b ~> IO c) :: a ~> IO c where
  k1 >=> k2 = KleisliComp k1 k2

data KleisliComp (k1 :: (a :: Type) ~> IO (b :: Type)) (k2 :: b ~> IO c) :: a ~> IO c
type instance KleisliComp k1 k2 $$ x = (k1 $$ x) >>= k2

-- hacks

type family Force x where
  Force Exit = Exit
  Force a = a

data TyFun a b
type a ~> b = TyFun a b -> Type
type family (f :: a ~> b) $$ (x :: a) :: b

-- Utilities from plain Haskell
type family f $ x where f $ x = f x
infixr 0 $

type family xs ++ ys where
    '[] ++ ys = ys
    (x ': xs) ++ ys = x ': (xs ++ ys)
