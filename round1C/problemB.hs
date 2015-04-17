{-# LANGUAGE BangPatterns #-}

import Control.Arrow
import Control.Monad
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.IO
import Debug.Trace

main = do
  cases <- parse . lines <$> hGetContents stdin
  forM_ (zip [(1::Int)..] (map solve cases)) $ \(i, sol) ->
    putStrLn $ "Case #" ++ show i ++ ": " ++ show sol

parse :: [String] ->  [[String]]
parse (numCasesStr:rest) = parse' rest

parse' (_:s:q) = words s : parse' q
parse' _ = []

madd, mmult :: Int64 -> Int64 -> Int64
madd a b = (a + b) `mod` 1000000007
mmult a b = (a * b) `mod` 1000000007

fact :: Int64 -> Int64
fact = f 1
  where f !c !n = if n == 0 then c else f (c `mmult` n) (n-1)

factorial :: (Integral a) => a -> Int64
factorial = fact . fromIntegral

-- naive solution, performance too bad even for the small input

bruteforce :: [String] -> Int64
bruteforce ss | length ss > 5 = -1
              | otherwise = fromIntegral . length .  filter valid . permutations $ ss
  where valid perm = let s = map head (group (concat perm))
                         s' = nub s
                     in if length s == length s' then trace (show perm) True else False

-- less naive solution

data Train = SingleLetter Char
           | MultiLetter Char (Set.Set Char) Char
           deriving (Eq, Show)

preprocess :: String -> Train
preprocess w = case group w of
  [ww] -> SingleLetter (head ww)
  w0:w1:ws -> MultiLetter (head w0)
                          (Set.fromList . map head . init $ w1:ws)
                          (head . last $ w1:ws)

solve :: [String] -> Int64
solve trains = fromMaybe 0 $ do
  let trains' = map preprocess trains
      trains'' = zip [0..] trains'

  let singles = map (head &&& length) . group . sort
                $ [c | (SingleLetter c) <- trains']
  middles <- err "middles"
             $ disjointUnion [c | (MultiLetter _ c _) <- trains']
  starts <- err "starts"
            $ fromListUnique [(c, i) | (i, MultiLetter c _ _) <- trains'']
  ends   <- err "ends"
            $ fromListUnique [(c, i) | (i, MultiLetter _ _ c) <- trains'']

  mapM_ (err "intermiddle" . guard . null . Set.intersection middles)
    $ [Map.keysSet starts, Map.keysSet ends, Set.fromList (map fst singles)]

  let startends = Map.intersectionWith (,) starts ends
      adjacency = Map.fromList . map snd . Map.toList $ startends
      
  err "cycle" . guard . not $ cyclic adjacency

  let factorsingle = foldl' mmult 1 [factorial n | (_, n) <- singles]
      factorperm = factorial bloup
      bloup = length [() | (MultiLetter c _ _) <- trains',
                      not (c `Map.member` ends)]
              + length [() | (c, _) <- singles,
                        not (c `Map.member` starts),
                        not (c `Map.member` ends)]

  return $ factorsingle `mmult` factorperm

disjointUnion :: (Ord a) => [Set.Set a] -> Maybe (Set.Set a)
disjointUnion = foldM f Set.empty
  where f acc this | not . Set.null $ acc `Set.intersection` this = Nothing
                   | otherwise = Just $ acc `Set.union` this

fromListUnique :: (Ord k) => [(k, v)] -> Maybe (Map.Map k v)
fromListUnique = sequenceA
                 . Map.fromListWith (\_ _ -> Nothing)
                 . map (second Just)

cyclic :: Map.Map Int Int -> Bool
cyclic m | Map.null m = False
         | otherwise = not . Map.null $ iterate sq m !! lg
  where lg = (+2) . length . takeWhile (/= 0) . iterate (`div` 2) . fst $ Map.findMax m
        sq mm = Map.mapMaybe (flip Map.lookup mm) mm

err = flip const

-- err :: a -> Maybe b -> Either a b
-- err e (Just x) = Right x
-- err e Nothing = Left e

