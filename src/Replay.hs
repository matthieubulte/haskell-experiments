{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Replay (main) where

import           Control.Monad (ap, (>=>))
import           Data.Maybe    (fromMaybe)
import           Data.Time     (UTCTime, diffUTCTime, getCurrentTime)

-- Isomorphismus stuff
class Inj a b where
  to :: a -> b

instance Inj a a where to = id
instance (Read a) => Inj String a where to = read
instance (Show a) => Inj a String where to = show

type Iso a b = (Inj a b, Inj b a)

from :: (Iso a b) => b -> a
from = to

-- Zipper stuff
data Zipper a = Zip [a] [a] deriving (Show)

empty :: Zipper a
empty = Zip [] []

append :: Zipper a -> a -> Zipper a
append (Zip l r) a = Zip l (r ++ [a])

pointed :: Zipper a -> Maybe a
pointed (Zip _ (r:_)) = Just r
pointed _             = Nothing

rewind :: Zipper a -> Zipper a
rewind (Zip l r) = Zip [] (reverse l ++ r)

shiftLeft :: Zipper a -> Zipper a
shiftLeft (Zip l (r:rs)) = Zip (r:l) rs
shiftLeft z = z

insertLeft :: a -> Zipper a -> Zipper a
insertLeft a (Zip l r) = Zip (a:l) r


type Trace = Zipper

data ReplayT q r m a where
  Return ::              a                             -> ReplayT q r m a
  Do     :: (Iso r b) => m b -> (b -> ReplayT q r m a) -> ReplayT q r m a
  Ask    ::              q   -> (r -> ReplayT q r m a) -> ReplayT q r m a

instance (Functor m) => Functor (ReplayT q r m) where
  fmap f (Return a) = Return (f a)
  fmap f (Do b c)   = Do b (fmap f . c)
  fmap f (Ask q c)  = Ask q (fmap f . c)

instance (Monad m) => Applicative (ReplayT q r m) where
  pure  = pure
  (<*>) = ap

instance (Monad m) => Monad (ReplayT q r m) where
  return = Return
  (Return a)   >>= f = f a
  (Do a cont)  >>= f = Do a (cont >=> f)
  (Ask q cont) >>= f = Ask q (cont >=> f)

lift :: (Iso r a, Monad m) => m a -> ReplayT q r m a
lift a = Do a return

ask :: (Monad m) => q -> ReplayT q r m r
ask q = Ask q return

run :: (Monad m) => ReplayT q r m a -> Trace r -> m (Either (q, Trace r) a)
run (Return a) _ = return (Right a)
run (Ask q cont) z  = case pointed z of
  Just p -> run (cont p) (shiftLeft z)
  _      -> return (Left (q, z))
run (Do a cont) z = case pointed z of
  Just p -> run ((cont . to) p) (shiftLeft z)
  _      -> a >>= \r -> run (cont r) (insertLeft (from r) z)

running :: (Monad m) => (q -> m r) -> ReplayT q r m a -> m a
running f p = play empty
  where
    play t = do
      r <- run p t
      case r of
        Left (q, t') -> f q >>= play . rewind . (t' `append`)
        Right x      -> return x

example :: ReplayT String String IO Int
example = do
  t0 <- lift getCurrentTime
  lift (putStrLn "Hello!")
  age <- ask "What is your age?"
  lift (putStrLn ("You are " ++ age))
  name <- ask "What is your name?"
  lift (putStrLn (name ++ " is " ++ age ++ " years old"))
  t1 <- lift getCurrentTime
  lift (putStrLn ("Total time: " ++ show (diffUTCTime t1 t0)))
  return (read age)

main :: IO ()
main = running (\q -> putStr (q ++ " ")  >> getLine) example >>= print
