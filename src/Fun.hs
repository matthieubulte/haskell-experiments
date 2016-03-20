module Fun where

import           Data.Char (toUpper)

newtype F r a = F { unF :: r -> a }

instance Functor (F r) where
  fmap g f = F (g . unF f)

{-
  identity : fmap id = id
  -----------------------
  fmap id v
= F (id . unF v)
= F (unF v)
= v

  composition : fmap (p . q) = fmap p . fmap q
  --------------------------------------------
  fmap (p . q) a
= F ((p . q) . unF a)
= F ((p . q) . unF a)
= F (p . (q . unF a))
= F (p . (unF . F) (q . unF a))
= F (p . unF (F (q . unF a))
= F (p . unF (fmap q a))
= fmap p (fmap q a)
= (fmap p . fmap q) a
-}

instance Applicative (F r) where
  pure    = F . const
  f <*> x = F (\a -> unF f a (unF x a))

{-
  identity : pure id <*> v = v
  ----------------------------
  pure id <*> v
= F (const id) <*> v
= F (\x -> unF (F (const id)) x (unF v x))
= F (\x -> const id x (unF v x)
= F (\x -> id (unF v x))
= F (\x -> unF v x)
= F (unF v)
= v

  composition : pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
  ----------------------------------------------------------
  pure (.) <*> u <*> v <*> w
= F (const (.)) <*> u <*> v <*> w
= F (\x -> unF (F (const (.))) x (unF u x)) <*> v <*> w
= F (\x -> const (.) x (unF u x)) <*> v <*> w
= F (\x -> (.) (unF u x)) <*> v <*> w
= F (\x -> ((.) . unF u) x) <*> v <*> w
= F ((.) . unF u) <*> v <*> w
= F (\x -> unF (F ((.) . unF u)) x (unF v x)) <*> w
= F (\x -> ((.) . unF u) x (unF v x)) <*> w
= F (\y -> unF (F (\x -> ((.) . unF u) x (unF v x))) y (unF w y))
= F (\y -> (\x -> ((.) . unF u) x (unF v x)) y (unF w y))
= F (\y -> ((.) . unF u) y (unF v y) (unF w y))
= F (\y -> (.) (unF u y) (unF v y) (unF w y))
= F (\y -> (unF u y . unF v y) (unF w y))
= F (\y -> unF u y (unF v y (unF w y)))
= F (\y -> unF u y ((\y -> unF v y (unF w y)) y))
= F (\y -> unF u y (unF (F (\x -> unF v y (unF w y))) x))
= F (\y -> unF u y (unF (v <*> w) y ))
= u <*> (v <*> w)

  homomorphism : pure f <*> pure x = pure (f x)
  ---------------------------------------------
  pure f <*> pure x
= F (const f) <*> F (const x)
= F (\y -> unF (F (const f)) y (unF (F (const x)) y))
= F (\y -> const f y (const x y))
= F (\y -> f (const x y))
= F (\y -> f x)
= F (const (f x))
= pure (f x)

  interchange : u <*> pure y = pure ($ y) <*> u
  ---------------------------------------------
  u <*> pure y
= u <*> F (const y)
= F (\x -> unF u x (unF (F (const y)) x))
= F (\x -> unF u x (const y x))
= F (\x -> unF u x y)
= F (\x -> ($ y) (unF u x))
= F (\x -> (const ($ y)) x (unF u x))
= F (\x -> unF (F (const ($ y))) x (unF u x))
= F (\x -> unF (pure ($ y)) x (unF u x))
= pure ($ y) <*> u
-}

instance Monad (F r) where
  return = pure
  f >>= g = F (\x -> unF (g (unF f x)) x)

{-
  left identity : return a >>= f = f a
  ------------------------------------
  return a >>= f
= pure a >>= f
= F (const a) >>= f
= F (\x -> unF (f (unF (F (const a)) x)) x)
= F (\x -> unF (f (const a x)) x)
= F (\x -> unF (f a x)
= F (unF . f a)
= f a

  right identity : m >>= return = m
  ---------------------------------
  m >>= return
= F (\x -> unF (return (unF m x)) x)
= F (\x -> unF (pure (unF m x)) x)
= F (\x -> unF (F (const (unF m x))) x)
= F (\x -> (const (unF m x)) x)
= F (\x -> unF m x)
= F (unF m)
= m

  associativity : (m >>= f) >>= g = m >>= (\x -> f x >>= g)
  ---------------------------------------------------------
  (m >>= f) >>= g
= F (\x -> unF (g (unF (m >>= f) x)) x)
= F (\x -> unF (g (unF (F (\y -> unF (f (unF m y)) y)) x)) x)
= F (\x -> unF (g ((\y -> unF (f (unF m y)) y) x)) x)
= F (\x -> unF (g (unF (f (unF m x)) x)) x)
= F (\x -> unF (g (unF (f (unF m x)) x)) x)
= F (\y -> unF (g (unF (f (unF m y)) y)) y))
= F (\y -> (\z -> unF (g (unF (f (unF m y)) z)) z)) y)
= F (\y -> unF (F (\z -> unF (g (unF (f (unF m y)) z)) z)) y)
= F (\y -> unF (f (unF m y) >>= g) y)
= F (\y -> unF ((\x -> f x >>= g) (unF m y)) y)
= m >>= (\x -> f x >>= g)
-}

type Reader = F

ask :: Reader r r
ask = F id

asks :: (r -> a) -> Reader r a
asks = (<$> ask)

local :: (r -> r) -> Reader r a -> Reader r a
local = withReader

withReader :: (r' -> r) -> Reader r a -> Reader r' a
withReader f m = F (unF m . f)

runReader :: Reader r a -> r -> a
runReader = unF

length' :: [a] -> Reader Int Int
length' []     = ask
length' (_:xs) = local (+1) (length' xs)
