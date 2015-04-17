{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Control.Monad
import Data.List
import Data.Int
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import System.IO
import Debug.Trace

main = do
  cases <- parse . lines <$> hGetContents stdin
  forM_ (zip [(1::Int)..] (map solve cases)) $ \(i, sol) ->
    putStrLn $ "Case #" ++ show i ++ ": " ++ show sol

showSol Nothing = "impossible"
showSol (Just n) = show n

parse :: [String] ->  [[String]]
parse (numCasesStr:rest) = parse' rest

parse' (_:s:q) = words s : parse' q
parse' _ = []

data Train = SingleLetter Char
           | MultiLetter Char (Set.Set Char) Char

preprocess w = case group w of
  [ww] -> SingleLetter (head ww)
  w0:w1:ws -> MultiLetter (head w0) (Set.fromList (map head (init (w1:ws)))) (head (last (w1:ws)))

madd a b = (a + b) `mod` 1000000007
mmult :: Int64 -> Int64 -> Int64
mmult a b = (a * b) `mod` 1000000007
factorial = f 1
  where f !c !n = if n == 0 then c else f (c `mmult` n) (n-1)

bruteforce :: [String] -> Int64
bruteforce ss | length ss > 5 = -1
              | otherwise = fromIntegral . length .  filter valid . permutations $ ss
  where valid perm = let s = map head (group (concat perm))
                         s' = nub s
                     in if length s == length s' then trace (show perm) True else False

solve :: [String] -> Int64
solve trains = g $ foldM f initial trains'
  where trains' = zip [0..] . map preprocess $ trains
        trains'' = Map.fromList trains'
        initial = (Map.fromList (zip ['a'..'z'] (repeat 0)), Map.empty, Set.empty, Map.empty)
        f (singles, starts, middles, ends) (ix, train) = case train of
          SingleLetter c -> Just (Map.adjust (1+) c singles, starts, middles, ends)
          MultiLetter start curMiddles end
            | start `Map.member` starts || start `Set.member` middles
              || end `Set.member` middles || end `Map.member` ends
              || any (\m -> m `Map.member` starts || m `Set.member` middles
                            || m `Map.member` ends) (Set.toList curMiddles)
              -> Nothing
            | otherwise ->
              Just (singles, Map.insert start ix starts,
                    curMiddles `Set.union` middles, Map.insert end ix ends)
        g Nothing = 0
        g (Just (singles, starts, _, ends))
          | let x = map snd . Map.toList $ startends in length (nub x) < length x = 0
          | otherwise = factorsingle `mmult` factorperm
          where factorsingle = Map.foldl' mmult 1 . Map.map factorial $ singles
                startends = Map.intersectionWith (,) starts ends
                
                bloup = Map.size (Map.filterWithKey
                                  (\k x -> x /= 0 &&
                                           not (k `Map.member` starts) &&
                                           not (k `Map.member` ends))
                                  singles)
                        + length (filter h trains') - Map.size startends
                factorperm = factorial . fromIntegral $ bloup
        h (_, SingleLetter _) = False
        h (_, MultiLetter _ _ _) = True

