{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Set as Set
import System.IO

main = do
  cases <- parse . lines <$> hGetContents stdin
  forM_ (zip [(1::Int)..] (map solve cases)) $ \(i, sol) ->
    putStrLn $ "Case #" ++ show i ++ ": " ++ showSol sol

showSol Nothing = "Fegla Won"
showSol (Just n) = show n

parse :: [String] -> [[String]]
parse (numCasesStr:rest) = parse' rest

parse' (sn:q) | length this < n = []
              | otherwise = this : parse' rest
  where n = read sn
        (this,rest) = splitAt n q
parse' [] = []

-- Solution for N >= 2
solve :: [String] -> Maybe Int
solve ss | any (/= length (head ss')) $ map length (tail ss') = Nothing
         | otherwise = fmap sum $ mapM f (transpose ss')
  where ss' = map group ss
        f xs | any (/= head (head xs)) (map head (tail xs)) = Nothing
             | otherwise = Just . sum . map (abs . ((-) x)) $ ls
          where ls = sort $ map length xs
                l = length xs
                x = ls !! (l `div` 2)

