import Control.Monad
import Data.List

data Result = Result Int | Bad | Inconceivable

showRes (Result n) = show n
showRes Bad = "Bad magician!"
showRes Inconceivable = "Volunteer cheated!"

main = do
  str <- readFile "A-small-attempt0.in"
  forM_ (zip [(1::Int)..] (solve . tail . lines $ str)) $ \(i, res) ->
    putStrLn $ "Case #" ++ show i ++ ": " ++ showRes res

solve linesFromFile =
  let (ls, rest) = splitAt 10 linesFromFile in
  if length ls < 10 then [] else
    let (x1s:q) = ls
        (rows1,q') = splitAt 4 q
        (x2s:q'') = q'
        (rows2, []) = splitAt 4 q''
        x1 = read x1s
        x2 = read x2s
        row1 = map read $ map words rows1 !! (x1 - 1)
        row2 = map read $ map words rows2 !! (x2 - 1)
    in case intersect row1 row2 of
      [] -> Inconceivable : solve rest
      [x] -> Result x : solve rest
      _ -> Bad : solve rest

