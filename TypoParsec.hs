{-# LANGUAGE DataKinds, TypeOperators, TypeInType, TypeFamilies, RankNTypes,
  UndecidableInstances, GADTs, ConstraintKinds, TypeApplications #-}
module TypoParsec where

import Prelude hiding (Char)
import GHC.TypeLits
type Char = Nat

type family a && b where
  True  && b = b
  False && _ = False
infixr 3 &&

type family If cond a b where
  If True  a _ = a
  If False _ b = b


data Parser a where
 Consume :: Parser Nat
 Return :: a -> Parser a
 Fail :: Parser a
 (:>>=) :: Parser b -> (b ~> Parser a) -> Parser a
 (:<|>) :: Parser a -> Parser a -> Parser a

type family RunParser (p :: Parser a) (s :: [Char]) :: Maybe (a, [Char])


type instance RunParser Consume (x ': xs) = Just '(x, xs)
type instance RunParser Consume '[] = Nothing
type instance RunParser (Return x) xs = Just '(x, xs)
type instance RunParser Fail _ = Nothing


data a ~> b where F :: sym -> (a ~> b)
type family Apply (f :: from ~> to) (x :: from) :: to
type family MkFun a b sym :: a ~> b where MkFun a b sym = F sym


type IsBetween lo hi = MkFun Char Bool (IsBetween_ lo hi)
data IsBetween_ lo hi
type instance Apply (F (IsBetween_ lo hi)) c = lo <=? c && c <=? hi



-- like 'Dict' from ekmett's constraints library
data Test c where Test :: c => Test c

isBetweenTests = Test @
  ( Apply (IsBetween 2 4) 1 ~ False
  , Apply (IsBetween 2 4) 2 ~ True
  , Apply (IsBetween 2 4) 3 ~ True
  , Apply (IsBetween 2 4) 4 ~ True
  , Apply (IsBetween 2 4) 5 ~ False
  )




type instance RunParser (p :>>= k) s = RunParserSeqHandleResult (RunParser p s) k
type family RunParserSeqHandleResult result k where
  RunParserSeqHandleResult (Just '(x, xs)) k = RunParser (Apply k x) xs
  RunParserSeqHandleResult Nothing         _ = Nothing


type BasicTest = Consume :>>= Const Consume Char

basicTest = Test @( RunParser BasicTest [1,2,3] ~ Just '( 2 , '[3]) )


-- Const x b :: a ~> b where x :: a
-- Apply (Const x t) y = x
-- where y :: t
type family Const x b where Const (x :: a) (b :: k) = MkFun b a (Const_ x)
data Const_ x
type instance Apply (F (Const_ x)) _ = x

-- p1 *> p2 -- parse p1 and p2, return result of p2
type family p1 *> p2 where (p1 :: Parser a) *> (p2 :: Parser b) = p1 :>>= Const p2 a

-- p1 <* p2 -- parse p1 and p2, return result of p1.
type family p1 <* p2 where
  (p1 :: Parser a) <* (p2 :: Parser b) = p1 :>>= MkFun a (Parser a) (FollowedByCont_ a b p2)

data FollowedByCont_ a b p2
type instance Apply (F (FollowedByCont_ a b p2)) x = p2 *> Return x

followedByTest = Test @( RunParser (CharP 1 <* CharP 2) '[1,2] ~ Just '( 1, '[] ) )

-- Fmap :: (a -> b) -> Parser a -> Parser b
type Fmap (f :: a -> b) (p :: Parser a) = p :>>= FmapCont f
type FmapCont (f :: a -> b) = MkFun a (Parser b) (FmapCont_ a b f)
data FmapCont_ a b (f :: a -> b)
type instance Apply (F (FmapCont_ a b f)) (x :: a) = Return (f x)

fmapTest = Test @( RunParser (Just `Fmap` Return 1) '[] ~ Just '( Just 1 , '[]) )

-- Ap :: Parser (a -> b) -> Parser a -> Parser b
type Ap (pf :: Parser (a -> b)) (pa :: Parser a) = pf :>>= MkFun (a -> b) (Parser b) (ApCont0 a b pa)
data ApCont0 a b pa; type instance Apply (F (ApCont0 a b pa)) (f :: a -> b) = Fmap f pa


fmapApTest = Test @( RunParser ( '(,) `Fmap` Consume `Ap` Consume ) [1,2,3]
  ~ Just '( '(1, 2), '[3] ) )


-- Satisfy :: (Char ~> Bool) -> Parser Char
-- Consume character satisfying given predicate and return it
type Satisfy pred = Consume :>>= MkFun Char (Parser Char) (SatisfyCont pred)
data SatisfyCont (pred :: Char ~> Bool)
type instance Apply (F (SatisfyCont pred)) (c :: Char) =
  If (Apply pred c)
     (Return c)
     Fail

-- CharP :: Char -> Parser Char
-- Consume exactly the given character
type CharP c = Satisfy (IsBetween c c)

charTest1 = Test @( RunParser (CharP 1) '[1] ~ Just '( 1, '[] ) )
charTest2 = Test @( RunParser (CharP 1) '[2] ~ Nothing )


type instance RunParser (a :<|> b) s = RunParserAltHandleResult (RunParser a s) b s
type family RunParserAltHandleResult result b s where
  RunParserAltHandleResult (Just r) _ _ = Just r
  RunParserAltHandleResult Nothing  b s = RunParser b s

alternativeTests = Test @
  ( RunParser (Return 1 :<|> Return 2) '[] ~ Just '( 1, '[] )
  , RunParser (Fail     :<|> Return 1) '[] ~ Just '( 1, '[] )
  -- failed branch shouldn't consume input
  , RunParser ((Consume *> Fail) :<|> Consume) '[1] ~ Just '( 1, '[] )
  )


-- Many p -- parse p until it fails, return list of results.
type family Many p where Many (p :: Parser a) = (p :>>= ManyCont a p) :<|> Return '[]
type ManyCont a (p :: Parser a) = MkFun a (Parser [a]) (ManyCont_ a p)
data ManyCont_ a p; type instance Apply (F (ManyCont_ a p)) x = Fmap ('(:) x) (Many p)

manyTest1 = Test @( RunParser (Many Consume) '[] ~ Just '( '[], '[] ) )
manyTest2 = Test @( RunParser (Many Consume) '[1] ~ Just '( '[1], '[] ) )
manyTest3 = Test @( RunParser (Many Consume) '[1,2] ~ Just '( '[1,2], '[] ) )
manyTest4 = Test @( RunParser (Many (CharP 1)) '[1,1,1,2] ~ Just '( '[1,1,1], '[2] ) )

type Many1 p = '(:) `Fmap` p `Ap` Many p



data SExpr = Atom [Char] | List [SExpr]


type IsAlpha = IsBetween 97 122 -- 'a'..'z'
type AtomP = Fmap Atom (Many1 (Satisfy IsAlpha))

atomTest1 = Test @( RunParser AtomP '[97,98,32] ~ Just '( Atom '[97,98], '[32] ) )

type IsSpace = IsBetween 32 32 -- ' '
type WhiteSpace = Many (CharP 32 :<|> CharP 13 :<|> CharP 10)

type LeftParenC = 40 -- '('
type RightParenC = 41 -- ')'

type ListP = Fmap List (CharP LeftParenC *> Many ( WhiteSpace *> LazySExprP ) <* CharP RightParenC)

listTest1 = Test @( RunParser ListP '[40,97,32,98,99,41]
   ~ Just '( List '[Atom '[97], Atom '[98,99]], '[] ) )
listTest2 = Test @( RunParser ListP '[40,41] ~ Just '( List '[], '[] ) )
listTest3 = Test @( RunParser ListP '[40,40,41,32,40,41,41]
  ~ Just '( List '[List '[], List '[]], '[] ) )

type family SExprP where SExprP = AtomP :<|> ListP



-- Lazy p -- works the same as `Apply p '()`.
type Lazy (p :: () ~> Parser a) = Return '() :>>= p
lazyTest1 = Test @( RunParser (Lazy (Const (Return 42) ())) '[] ~ Just '( 42, '[] ) )

type family LazySExprP :: Parser SExpr where LazySExprP = Lazy (F "SExprP")
type instance Apply (F "SExprP") '() = SExprP


-- "(a (c d) j kl)"    ->    List [Atom "a", List [Atom "c", Atom "d"], Atom "j", Atom "kl"]
sexprTest1 = Test @( RunParser SExprP '[40,97,32,40,99,32,100,41,32,32,106,107,41]
  ~ Just '( List '[Atom '[97], List '[Atom '[99], Atom '[100]], Atom '[106,107]], '[] ) )

-- "(a b" -> failure
sexprTest2 = Test @( RunParser SExprP '[40,97,32,98] ~ Nothing )

-- "1" -> failure
sexprTest3 = Test @( RunParser SExprP '[49] ~ Nothing )
