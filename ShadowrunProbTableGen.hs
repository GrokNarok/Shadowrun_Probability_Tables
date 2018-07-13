import           Data.List
import           Data.List.Split
import qualified Data.Vector as Vec
import qualified Data.Matrix as Mat
import           Numeric

import System.IO
import Data.Char

-- Gets binomial coefficient (here n are trials and k are successes, see getProbMass)
binomial :: (Integral i) => i -> i -> i
binomial n 0 = 1
binomial 0 k = 0
binomial n k = binomial (n-1) (k-1) * n `div` k 

-- Gets probability of exactly k successes in n trials with probability of success p
getProbMass :: (Integral i, Floating f) => i -> i -> f -> f
getProbMass k n p = fromIntegral (binomial n k) * p^^k * (1-p)^^(n-k)

-- Gets probability mass distribution for n trials with probability of success p
getDistr :: (Integral i, Floating f) => i -> f -> [f]
getDistr n p = map (\ k -> getProbMass k n p) [0..n]

-- Multiplies a one-column and a one-row matrices with input matrices represented by 2 lists
matMult :: Num a => [a] -> [a] -> Mat.Matrix a
matMult a b = Mat.multStd (Mat.colVector $ Vec.fromList $ b) (Mat.rowVector $ Vec.fromList $ a)

-- Gets a specific matrix diagonal specified by x, x=0 gets true diagonal x=1 gets diagonal starting at (1,0) element, x=-1 - (0,1) elem etc.
getDiag:: Int -> Mat.Matrix a -> Vec.Vector a
getDiag x m = 
 if x >= 0
  then
   Vec.generate k (\i -> Mat.getElem (i+1) (i+x+1) m)
  else
   Vec.generate j (\i -> Mat.getElem (i-x+1) (i+1) m)
 where
  k = min ((Mat.ncols m) - x) (Mat.nrows m)
  j = min (Mat.ncols m) ((Mat.nrows m) + x)

-- Compares two probability mass distributions and calculates the odds that distr A will get more successes then distr B
-- (number of net successes looked at is defined by r as a range, e.g. r=[0..2] will get odds of 0,1 and 2 net successes)
compareDistr ::  Num a => [a] -> [a] -> [Int] -> [a]
compareDistr a b [] = compareDistr a b [((length b) * (-1) + 1)..((length a) - 1)]
compareDistr a b r = map (\x -> sum $ getDiag x m) r where m = matMult a b

-- 
helper :: Num a => a -> [a] -> [a]
helper x [] = [x]
helper x acc = (x + (head acc)):acc

-- Same as compareDistr but calculates odds of [number] or more net successes
compareDistrGE ::  Num a => [a] -> [a] -> [Int] -> [a]
compareDistrGE a b [] = compareDistrGE a b [((length b) * (-1) + 1)..((length a) - 1)]
compareDistrGE a b r = take x $ foldr helper [] $ compareDistr a b [(head r)..((length a) - 1)] where x = (last r) - (head r) + 1

-- Takes a list of floats and if the list is shorter then n adds 0.0s up to n
padListTo :: Floating f => Int -> [f] -> [f]
padListTo n xs = take n (xs ++ repeat 0.0)

-- Gets probability mass distribution for getting one specific outcome rolling n dice with x sides (write as 4`d`6 - four 6-sided dice)
d :: (Integral i, Floating f) => i -> i -> [f]
d n x = getDistr n (1 / fromIntegral x)

-- Get the odds of getting [0..r] net hits on an opposed roll of d3 pools of size x and y
compareD3PoolsR a b r = compareDistrGE (a`d`3) (b`d`3) r

-- Get the odds of getting [0..4] net hits on an opposed roll of d3 pools of size x and y
compareD3PoolsTo4 a b = padListTo 5 $ compareD3PoolsR a b [0..4]

-- Get the odds of getting [0..4] net hits on opposed rolls of all combination of die pools sizes in ranges xr and yr
compareD3PoolsOver xr yr = chunksOf (length xr) $ (flip compareD3PoolsTo4) <$> yr <*> xr

-- Takes a Float and Turns it into a pretty string for printing
formatPercent f
 | p >= 99.9 = showFFloat (Just 0) p ""
 | p >= 9.99 = showFFloat (Just 1) p ""
 | otherwise = showFFloat (Just 2) p ""
 where p = f * 100

-- TeX generating code
makeCell :: [[Char]] -> [Char]
makeCell xs = " & $\\frac{\\textbf{" ++ xs!!0 ++ "/" ++ xs!!1 ++ "}}{" ++ xs!!2 ++ "/" ++ xs!!3 ++ "/" ++ xs!!4 ++ "}$"

makeRow :: Int -> [[[Char]]] -> [Char]
makeRow n xs = (show n) ++ (concat $ map makeCell xs) ++ " \\\\\n\\hline\n"

makeTable :: [Int] -> [Int] -> [Char]
makeTable xr yr =
    "\\documentclass{slides}\n"
 ++ "\\usepackage{graphicx}\n"
 ++ "\\usepackage{slashbox}\n"
 ++ "\\usepackage{multirow}\n"
 ++ "\\usepackage[margin=3mm]{geometry}\n"
 ++ "\\usepackage[table]{xcolor}\n"
 ++ "\\begin{document}\n"
 ++ "\\rowcolors{2}{white}{blue!10}\n"
 ++ "{\\renewcommand{\\arraystretch}{1.60}\\rotatebox{90}{\\resizebox{260mm}{!}{\\begin{tabular}{|" ++ (concat $ take ((length xr)+1) $ repeat "c|") ++ "}\n"
 ++ "\\multicolumn{" ++ (show $ (length xr)+1) ++ "}{c}{Each cell gives probabilities for getting x or more net hits on opposing roll, where x is 0-4, as following: $\\frac{\\textbf{0/1}}{2/3/4}$}\\\\"
 ++ "\\hline\n"
 ++ "\\rowcolor{blue!15}\n"
 ++ "\\backslashbox{Them}{You}" ++ (concat $ take (length xr) $ map (\b -> " & " ++ show b) ([(head xr)..])) ++ " \\\\\n"
 ++ "\\hline\n"
 ++ (concat $ zipWith makeRow [(head yr)..] $ (map.map.map) formatPercent $ compareD3PoolsOver xr yr)
 ++ "\\end{tabular}}}}\n"
 ++ "\\end{document}\n"

writeTeXFile name xr yr = writeFile name $ makeTable xr yr

main = writeTeXFile "probs1.tex" [1..10] [1..30] >> writeTeXFile "probs2.tex" [11..20] [1..30] >> writeTeXFile "probs3.tex" [21..30] [1..30]

-- Functions for getting intermediate results in a more readable format. see the readme.md for usage
getDistrForD3Pool n = map formatPercent $ n`d`3
getDistrForD3PoolGE n = map formatPercent $ foldr helper [] $ n`d`3
compareD3Pools a b = map formatPercent $ compareDistr (a`d`3) (b`d`3) [0..a]
compareD3PoolsGE a b = map formatPercent $ compareDistrGE (a`d`3) (b`d`3) [0..a]