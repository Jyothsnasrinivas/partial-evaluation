{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad
import Data.Array.IO
import Data.Array.Base
import Criterion.Main

data Expr = Num !Int !Double
          | Plus !Int !Int !Int
          | Decr !Int
          | Copy !Int !Int
          | Block ![Expr]
          | Loop !Int !Expr

main :: IO ()
main = do

  
  let interpretFib = interpret fib
  let partialEvalFib = partialEval fib

  print $ "native:      " ++ (show (fibonacci 10))
  iFib10 <-  runFib interpretFib 10
  print $ "interpret:   " ++ show iFib10
  pFib10 <- runFib partialEvalFib 10
  print $ "partialEval: " ++ show pFib10
  
  let n = 5e5
  
  defaultMain [
    bgroup "fib" [ bench "native" $ whnf fibonacci (2 * n)
                 , bench "interpret" $ nfIO $ runFib interpretFib n
                 , bench "partialEval" $ nfIO $ runFib partialEvalFib n
                 ]
      ]

fibonacci :: Double -> Double
fibonacci n = let
  fib !n !i !j | n == 0.0  = i 
            | otherwise = fib (n - 1.0) j (i+j)
  in fib n 0.0 1.0

interpret :: Expr -> IOUArray Int Double -> IO ()
interpret e m = case e of
  
  Num d n -> unsafeWrite m d n
  Decr d -> do
    i <- unsafeRead m d
    unsafeWrite m d (i-1)
  Plus d i j -> do
     iv <- unsafeRead m i
     jv <- unsafeRead m j
     unsafeWrite m d (iv + jv)
  Copy d i -> do
      iv <- unsafeRead m i
      unsafeWrite m d iv
  Loop haltIf0 p -> interpretLoop haltIf0 p
  Block es -> interpretBlock es
  
  where
    
    interpretLoop haltIf0 p = loop where
      loop = do
        c <- unsafeRead m haltIf0
        when (c /= 0) $ interpret p m *> loop
        
    interpretBlock (e:[]) = interpret e m
    interpretBlock (e:es) = let
      eio  = interpret e m
      esio = interpretBlock es in
      eio *> esio
      

partialEval :: Expr -> IOUArray Int Double -> IO ()
partialEval e = case e of
  
  Num d n -> \m -> unsafeWrite m d n
  Decr d -> \m -> do
    i <- unsafeRead m d
    unsafeWrite m d (i-1)
  Plus d i j -> \m -> do
     iv <- unsafeRead m i
     jv <- unsafeRead m j
     unsafeWrite m d (iv + jv)
  Copy d i -> \m -> do
      iv <- unsafeRead m i
      unsafeWrite m d iv
  Loop haltIf0 p -> partialEvalLoop haltIf0 $ partialEval p
  Block es -> partialEvalBlock es
  
  where

    partialEvalBlock (e:[]) = partialEval e
    partialEvalBlock (e:es) = let
      ep  = partialEval e
      esp = partialEvalBlock es in
      \m -> ep m *> (esp m)      

    partialEvalLoop :: Int -> (IOUArray Int Double -> IO ()) -> IOUArray Int Double -> IO ()
    partialEvalLoop haltIf0 p = loop where
      loop m = do
        c <- unsafeRead m haltIf0
        when (c /= 0) $ p m *> loop m

fib = Block [
    Num 1 0.0,       
    Num 2 1.0,
    Loop 0 $ Block [
        Plus 3 1 2,
        Copy 1 2,
        Copy 2 3,
        Decr 0
        ]
    ]

runFib p n = do
  m <- newArray_ (1, 4)
  unsafeWrite m 0 n
  p m
  unsafeRead m 1
