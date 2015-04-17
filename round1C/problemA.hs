{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Control.Monad
import Data.List
import Data.Ratio
import System.IO

main = do
  cases <- parse . lines <$> hGetContents stdin
  forM_ (zip [(1::Int)..] (map solve cases)) $ \(i, sol) ->
    putStrLn $ "Case #" ++ show i ++ ": " ++ showSol sol

showSol Nothing = "impossible"
showSol (Just n) = show n

parse :: [String] ->  [(Integer, Integer)]
parse (numCasesStr:rest) = map parseLine . take (read numCasesStr) $ rest

parseLine :: String -> (Integer, Integer)
parseLine str = let (s, '/':s') = break (== '/') str in
  (read s, read s')

log2Strict 1 = Just 0
log2Strict n | n `mod` 2 /= 0 = Nothing 
             | otherwise = (1+) <$> log2Strict (n `div` 2)

log2Int 1 = 0
log2Int n = 1 + log2Int (n `div` 2)

solve :: (Integer, Integer) -> Maybe Int
solve (p,q) | r > 1 = Nothing
            | otherwise = (flip (-) (log2Int p')) <$> (log2Strict q')
  where r = p % q
        g = gcd q p
        q' = q `div` g
        p' = p `div` g

