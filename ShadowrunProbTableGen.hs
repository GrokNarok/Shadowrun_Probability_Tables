import Data.List
import Data.List.Split
import Numeric
import System.IO
import System.Environment 
import Control.Exception
import Data.Char

data RollStyle = Normal | Reroll | Exploding deriving (Eq, Show, Read, Enum) 

-- Multinomial distribution probability mass function
-- Gets probability of exact list of outcomes xs in n trials with probabilities of success ps
getProbMass :: (Integral i, Floating f) => i -> [f] -> [i] -> f
getProbMass n ps xs = (fromIntegral coeff) * (product $ zipWith (^^) ps xs)
                      where coeff = (product [2..n]) `div` (product $ map (\x -> product [2..x]) xs)

-- Gets probability mass distribution for n trials with probability of success p
getDistr :: (Integral i, Floating f) => i -> f -> [f]
getDistr n p = map (\k -> getProbMass n [(1-p), p] [(n-k), k]) [0..n]

-- Generate a list of probabilities of [0,1,..] hits when rolling an exploding die with the chance p
-- to produce a hit on each roll and chance e of the die exploding (Note: this is an infinite list)
explosiveDice :: (Floating f) => f -> f -> [f]
explosiveDice p e = (1-p):[p*(e^^x - e^^(x+1)) | x <- [0,1..]]

-- This is a copy of _partition function from Math.Combinat.Partitions.Integer because I can't get it to compile.
partitionInt :: (Integral i) => i -> [[i]]
partitionInt d = go d d where go _ 0 = [[]]
                              go h n = [ a:as | a <- [1..min n h], as <- go a (n-a) ]

-- Gets probability mass distribution for exploding die pool of size n with probability of hit p and probability of explosion e
getExplosiveDistr :: (Integral i, Floating f) => i -> f -> f -> [f]
getExplosiveDistr n p e = map (\k -> sum $ map (getProbMass n $ explosiveDice p e) $ convertedParts k) [0..]
                          where partsUnderLength h = filter (\x -> (genericLength x) <= n) $ partitionInt h
                                convertedParts h = map (\xs -> (n-(sum xs)):xs) $ map reverse $ map (go h) $ map (groupBy (==)) $ partsUnderLength h
                                go 0 _  = [] -- This will convert a list like [4,2,1,1,1] into list like [0,3,1,0,1]
                                go s [] = 0:(go (s-1) [])
                                go s xss@(xs@(x:_):next)
                                 | s == x    = (genericLength xs):(go (s-1) next)
                                 | otherwise = 0:(go (s-1) xss)

-- Compares two probability mass distributions and calculates the odds that distr A will get more successes then distr B
-- (number of net successes looked at is defined by r as a range, e.g. r=[0..2] will get odds of 0,1 and 2 net successes)
compareDistr ::  Num a => [Int] -> [a] -> [a] -> [a]
compareDistr [] a b = compareDistr [((length b) * (-1) + 1)..((length a) - 1)] a b
compareDistr r a b = map (\x -> sum $ zipWith (*) (drop x a) b) r

-- Takes a list and replaces every element with the sum of it and all elements that follow it.
convertToGE :: Num a => [a] -> [a]
convertToGE []     = []
convertToGE (x:xs) = (x + sum xs):(convertToGE xs)

-- Get the odds of getting [0..r] net hits or more on an opposed roll of d3 pools of size x and y
compareD3Pools :: (Integral i, Floating f) => [Int] -> RollStyle -> RollStyle -> i -> i -> [f]
compareD3Pools r rsA rsB a b = map (comparison!!) r
                               where comparison = convertToGE $ compareDistr range (makeDistr rsA a) (makeDistr rsB b)
                                     range = [0..(max (fromIntegral a) (last r))]
                                     makeDistr rs n = case rs of Normal    -> getDistr n (1/3)
                                                                 Reroll    -> getDistr n (5/9)
                                                                 Exploding -> getExplosiveDistr n (1/3) (1/6)

-- Get the odds of getting [0..4] net hits on opposed rolls of all combination of die pools sizes in ranges xr and yr
generateTableValues :: (Integral i, Floating f) => RollStyle -> RollStyle -> [i] -> [i] -> [[[f]]]
generateTableValues rsA rsB xr yr = chunksOf (length xr) $ (flip $ compareD3Pools [0..4] rsA rsB) <$> yr <*> xr

-- Takes a Float and Turns it into a pretty string for printing
formatPercent :: (RealFloat f) => f -> String
formatPercent f
 | p >= 99.9 = showFFloat (Just 0) p ""
 | p >= 9.99 = showFFloat (Just 1) p ""
 | otherwise = showFFloat (Just 2) p ""
 where p = f * 100

-- TeX generating code
makeCell :: (Integral i) => RollStyle -> RollStyle -> i -> i -> String
makeCell rsA rsB y x = " & $\\frac{\\textbf{" ++ xs!!0 ++ "/" ++ xs!!1 ++ "}}{" ++ xs!!2 ++ "/" ++ xs!!3 ++ "/" ++ xs!!4 ++ "}$"
                 where xs = map formatPercent $ compareD3Pools [0..4] rsA rsB x y

makeRow :: (Integral i, Show i) => RollStyle -> RollStyle -> [i] -> i -> String
makeRow rsA rsB xr y = (show y) ++ (concat $ map (makeCell rsA rsB y) xr) ++ " \\\\\n\\hline\n"

makeTable :: (Integral i, Show i) => RollStyle -> RollStyle -> [i] -> [i] -> String
makeTable rsA rsB xr yr =
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
 ++ "\\backslashbox{Them}{You}" ++ (concat $ take (length xr) $ map (\b -> " & " ++ show b) xr) ++ " \\\\\n"
 ++ "\\hline\n"
 ++ (concat $ map (makeRow rsA rsB xr) yr)
 ++ "\\end{tabular}}}}\n"
 ++ "\\end{document}\n"

-- User input processing functions
tablesFromArgs :: IO ()
tablesFromArgs = do (name:rsA:a:b:rsB:c:d:[]) <- getArgs
                    writeFile name $ makeTable (read rsA) (read rsB) [(read a) .. (read b)] [(read c) .. (read d)]
                    putStrLn $ "Probability table for " ++ a ++ "-" ++ b ++ " x " ++ c ++ "-" ++ d ++ " die pools generated."

defaultTables :: IOError -> IO ()
defaultTables _ = writeTeXFile "probs1.tex" [1..10] [1..30] >> writeTeXFile "probs2.tex" [11..20] [1..30] >> writeTeXFile "probs3.tex" [21..30] [1..30] >>
                  putStrLn "Standard probability tables generated (No parameters or invalid parameters provided)."
                  where writeTeXFile name xr yr = writeFile name $ makeTable Normal Normal xr yr

main = tablesFromArgs `catch` defaultTables

-- Functions for getting intermediate results in a more readable format (See the readme.md for usage)
getDistrForD3Pool n = map formatPercent $ n`d`3
getDistrForD3PoolGE n = map formatPercent $ convertToGE $ n`d`3
getComparisonOfD3Pools a b = map formatPercent $ compareDistr [0..a] (a`d`3) (b`d`3)
getComparisonOfD3PoolsGE a b = map formatPercent $ convertToGE $ compareDistr [0..a] (a`d`3) (b`d`3)

-- Gets probability mass distribution for getting one specific outcome rolling n dice with x sides (write as 4`d`6 - four 6-sided dice)
d :: (Integral i, Floating f) => i -> i -> [f]
d n x = getDistr n (1 / fromIntegral x)