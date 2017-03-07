{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds,
             TypeOperators, UndecidableInstances #-}

data Nat = Z | S Nat

type family Plus (a :: Nat) (b :: Nat) :: Nat where
  Plus Z n = n
  Plus (S n) k = S (Plus n k)

type One = 'S 'Z
type Two = 'S ('S 'Z)
type Three = S (S (S Z))
