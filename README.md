# Shadowrun_Probability_Tables

Here you'll find probability tables (in jpeg, pdf and TeX) for opposed rolls in [Shadowrun 5e](https://en.wikipedia.org/wiki/Shadowrun) and Haskell script that was used to generate them. If you don't play Shadowrun you probably won't find this too relevant.

Quick Shadowrun rule summary:<br />
Contests in Shadowrun are resolved by rolling a number of d6 (six-sided) dice and counting "hits" - 5s or 6s rolled, more hits is better. A Player and the Game Master both roll a set of dice and count hits scored, whoever has more hits wins the contest. More "net hits" (your hits minus opponent's hits) constitutes a higher degree of success. The tables generated here answer a question like "What is the chance I will get 1 or more net hits if I'm rolling 14 dice and the opponent is rolling 11 dice?" (it's 58.3%)

Using the script:<br />
Just load the script in ghci.

Useful functions:<br />
`writeTeXFile "fileName" \[player die pool range] \[opponent die pool range]`  (e.i. `writeTeXFile "probs1.tex" \[1..10] \[1..30]`)
This will generate a table in .TeX format and write into a file under provided name. The table has the probabilities for opposed rolls between die pools of sizes specified by the ranges. Any ranges can be specified but the TeX is optimised for 10 by 30 size as that fits on a page nicely without been to small to read. You'll need a loot to read TeX files or convert them to other formats, I used `TeXstudio`.<br />
To regenerate the tables uploaded to this repo type:<br />
`writeTeXFile "probs1.tex" \[1..10] \[1..30]`<br />
`writeTeXFile "probs2.tex" \[11..20] \[1..30]`<br />
`writeTeXFile "probs3.tex" \[21..30] \[1..30]`<br />
<br />
`compareD3Pools x y` (e.i. `compareD3PoolsR 5 3`)<br />
This will print a list of probabilities of getting net hits, staring at 0 net hits (tie), on an opposed roll between die pools of sizes x and y. <br />
<br />
`getDistrForD3Pool x` (e.i. `getDistrForD3Pool 8`)<br />
This will print a list of probabilities of getting hits, staring at 0 hits, with a die pool of size x.<br />
