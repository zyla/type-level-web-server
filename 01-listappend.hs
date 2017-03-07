{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds,
             TypeOperators, UndecidableInstances #-}

type family (xs :: [a]) ++ (ys :: [a]) :: [a] where
  '[] ++ ys       = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)
