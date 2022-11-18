{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Main where
import Data.Propagator
import GHC.ST ( ST )
import Data.Propagator.Supported (Supported)

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
