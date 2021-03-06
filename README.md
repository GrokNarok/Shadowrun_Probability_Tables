# Shadowrun_Probability_Tables

Final tables can be found [here](https://imgur.com/a/lrGKDF1).

Here you'll find probability tables (in jpeg, pdf and TeX) for opposed rolls in [Shadowrun 5e](https://en.wikipedia.org/wiki/Shadowrun) and Haskell script that was used to generate them. If you don't play Shadowrun you probably won't find this too relevant.

Quick Shadowrun rule summary:<br />
Contests in Shadowrun are resolved by rolling a number of d6 (six-sided) dice and counting "hits" - 5s or 6s rolled, more hits is better. A Player and the Game Master both roll a set of dice and count hits scored, whoever has more hits wins the contest. More "net hits" (your hits minus opponent's hits) constitutes a higher degree of success. The tables generated here answer a question like "What is the chance I will get 1 or more net hits if I'm rolling 14 dice and the opponent is rolling 11 dice?" (it's 58.3%)

**Using the script:**<br />
Compile/interpret `ShadowrunProbTableGen.hs`.<br />
Run `ShadowrunProbTableGen "OutputFileName.tex" [Normal|Reroll] [player die pool low bound] [player die pool high bound] [Normal|Reroll|Exploding] [opponent die pool low bound] [opponent die pool high bound]`<br />
(e.i. `ShadowrunProbTableGen "myProbs.tex" Reroll 1 10 Normal 1 30`, note capitalisation)<br />
This will generate a table in .TeX format and write into a file under provided name. The table has the probabilities for opposed rolls between die pools of sizes specified by the ranges. `Normal` is for a normal roll, `Reroll` is for a roll with one reroll of misses, `Exploding` is for a roll where each die that rolls a 6 is rerolled and results are added, multiple rerolls per die are possible (WARNING: this only works for the opponent die pool right now). Any ranges can be specified but the TeX is optimised for 10 by 30 size as that fits on a page nicely without been to small to read. You'll need a tool to read TeX files or convert them to other formats, I used `TeXstudio`.<br />
<br />
To regenerate the tables uploaded to this repo type:<br />
`ShadowrunProbTableGen "probs1.tex" Normal 1 10 Normal 1 30`<br />
`ShadowrunProbTableGen "probs2.tex" Normal 11 20 Normal 1 30`<br />
`ShadowrunProbTableGen "probs3.tex" Normal 21 30 Normal 1 30`<br />
`ShadowrunProbTableGen "probsReroll1.tex" Reroll 1 10 Normal 1 30`<br />
`ShadowrunProbTableGen "probsReroll2.tex" Reroll 11 20 Normal 1 30`<br />
`ShadowrunProbTableGen "probsReroll3.tex" Reroll 21 30 Normal 1 30`<br />
<br />
Oterwise you can load `ShadowrunProbTableGen.hs` into `ghci` to get access to following useful functions:<br />
<br />
`getComparisonOfD3Pools x y` (e.i. `getComparisonOfD3Pools 5 3`)<br />
This will print a list of probabilities of getting net hits, staring at 0 net hits (tie), on an opposed roll between die pools of sizes x and y. <br />
`getComparisonOfD3PoolsGE` same as `getComparisonOfD3Pools` but gives probabilities for "x or more" hits instead of exact hits.<br />
<br />
`getDistrForD3Pool x` (e.i. `getDistrForD3Pool 8`)<br />
This will print a list of probabilities of getting hits, staring at 0 hits, with a die pool of size x.<br />
`getDistrForD3PoolGE` same as `getDistrForD3Pool` but gives probabilities for "x or more" hits instead of exact hits.<br />
