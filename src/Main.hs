{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# language RankNTypes #-}
{-# HLINT ignore "Use <$>" #-}
module Main where
import Data.Propagator
import GHC.ST ( ST, runST )
import Data.Propagator.Supported (Supported)

runNetwork :: Show a => (forall s . ST s a) -> IO ()
runNetwork st = print $ runST st

main :: IO ()
main = 
        do 
                putStrLn "Hello world!"

test :: Supported Int
test = 
        do 
           return $ fromInteger 2

test1 :: ST s (Cell s (Supported Int))
test1 = known test


adderBackwardExample :: ST s (Maybe Int)
adderBackwardExample = do
  inL <- cell
  inR <- cell
  out <- cell

  adder inL inR out

  write out 10
  write inL 3

  content inR

adder :: Cell s Int -> Cell s Int -> Cell s Int -> ST s ()
adder inL inR out = do
  watch inL $ \l -> do
    with inR $ \r -> write out (l+r)
    with out $ \o -> write inR (o-l)
  watch inR $ \r -> do
    with inL $ \l -> write out (l+r)
    with out $ \o -> write inL (o-r)
  watch out $ \o -> do
    with inR $ \r -> write inL (o-r)
    with inL $ \l -> write inR (o-l)


inBTree :: Either () (b,(BTree b,BTree b)) -> BTree b
inBTree = either (const Empty) Node

outBTree :: BTree a -> Either () (a,(BTree a,BTree a))
outBTree Empty              = Left ()
outBTree (Node (a,(t1,t2))) = Right(a,(t1,t2))

baseBTree f g = id -|- (f >< g)

cataBTree g = g . (recBTree (cataBTree g)) . outBTree

anaBTree g = inBTree . (recBTree (anaBTree g) ) . g

hyloBTree h g = cataBTree h . anaBTree g

recBTree f = baseBTree id f    