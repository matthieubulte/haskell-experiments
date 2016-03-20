{-# LANGUAGE GADTs #-}

module Main where

import           Control.Monad (ap, (>=>))
import           Data.Time

type Trace q = [q]
singleton a = [a]


data Replay q r a where
  Return :: a                           -> Replay q r a
  Do     :: IO b -> (b -> Replay q r a) -> Replay q r a
  Ask    :: q    -> (r -> Replay q r a) -> Replay q r a

instance Functor (Replay q r) where
  fmap f (Return a) = Return (f a)
  fmap f (Do b c)   = Do b (fmap f . c)
  fmap f (Ask q c)  = Ask q (fmap f . c)

instance Applicative (Replay q r) where
  pure  = return
  (<*>) = ap

instance Monad (Replay q r) where
  return = Return
  (Return a) >>= f = f a
  (Do a c)   >>= f = Do a (c >=> f)
  (Ask q c)  >>= f = Ask q (c >=> f)

io :: IO a -> Replay q r a
io a = Do a return

ask :: q -> Replay q r r
ask q = Ask q return

run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run (Return a)     _ = return (Right a)
run (Ask q c)     [] = return (Left (q, []))
run (Ask _ c) (r:rs) = run (c r) rs
run (Do a c)      rs = fmap c a >>= (`run` rs)

running :: Replay String String a -> IO a
running prog = play mempty
 where
  play t = do
    r <- run prog t    -- this is the same prog every time!
    case r of
      Left (q,t') -> do
        putStr ("Question: " ++ q ++ " ")
        r <- getLine
        play (t' `mappend` singleton r)
      Right x -> return x

example :: Replay String String Int
example = do
  t0 <- io getCurrentTime
  io (putStrLn "Hello!")
  age <- ask "What is your age?"
  io (putStrLn ("You are " ++ age))
  name <- ask "What is your name?"
  io (putStrLn (name ++ " is " ++ age ++ " years old"))
  t1 <- io getCurrentTime
  io (putStrLn ("Total time: " ++ show (diffUTCTime t1 t0)))
  return (read age)


main :: IO ()
main = running example >>= print
