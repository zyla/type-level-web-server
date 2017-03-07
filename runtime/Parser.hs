module Parser where

import Data.Char
import Text.Parsec
import Text.Parsec.String

type Cont = String
type Bytes = [Int]

data Operation =
    Exit
  | Listen Int      Cont
  | Accept Int      Cont
  | Write Int Bytes Cont
  | Read Int Int    Cont
  | Open Bytes      Cont
  | Close Int       Cont
  deriving (Eq, Show)

pOperation :: Parser Operation
pOperation = (string "'" >>) $
     (Exit   <$ string "Exit")
 <|> (Listen <$ string "Listen") <*> nat <*> cont
 <|> (Accept <$ string "Accept") <*> nat <*> cont
 <|> (Write  <$ string "Write") <*> nat <*> bytes <*> cont
 <|> (Read   <$ string "Read") <*> nat <*> nat <*> cont
 <|> (Open   <$ string "Open") <*> bytes <*> cont
 <|> (Close  <$ string "Close") <*> nat <*> cont

cont = ws *> many (anyChar)
bytes = ws *> string "'[" *> (nat `sepBy` (ws *> char ',')) <* char ']'
ws = many (satisfy isSpace)

nat = ws *> intLit
intLit = read' {- HACK -} <$> many1 (satisfy isDigit)

read' x = case reads x of
            [(val, "")] -> val
            _           -> error $ "parse error: " ++ x

parseOperation :: String -> Either ParseError Operation
parseOperation = parse pOperation ""
