# Code for paper: Identifying Copeland Winners in Dueling Bandits with Indifferences

This repository holds the code for our paper "Identifying Copeland Winners in Dueling Bandits with Indifferences" by Viktor Bengs, Björn Haddenhorst and Eyke Hüllermeier. Regarding questions please contact viktor.bengs@ifi.lmu.de .

ArXiv preprint: [https://arxiv.org/abs/2109.06234](https://arxiv.org/abs/2310.00750)

Published paper (open access): [https://proceedings.mlr.press/v238/bengs24a.html](https://proceedings.mlr.press/v238/bengs24a.html)

Please cite this work as

```
@InProceedings{bengs2024ident,
  title = 	 {Identifying {C}opeland Winners in Dueling Bandits with Indifferences},
  author =       {Bengs, Viktor and Haddenhorst, Bj\"{o}rn and H\"{u}llermeier, Eyke},
  booktitle = 	 {Proceedings of The 27th International Conference on Artificial Intelligence and Statistics},
  pages = 	 {226--234},
  year = 	 {2024},
  volume = 	 {238},
  series = 	 {Proceedings of Machine Learning Research},
  publisher =    {PMLR},
}

```

## Abstract
We consider the task of identifying the Copeland winner(s) in a dueling bandits problem with ternary feedback. This is an underexplored but practically relevant variant of the conventional dueling bandits problem, in which, in addition to strict preference between two arms, one may observe feedback in the form of an indifference. We provide a lower bound on the sample complexity for any learning algorithm finding the Copeland winner(s) with a fixed error probability. Moreover, we propose POCOWISTA, an algorithm with a sample complexity that almost matches this lower bound, and which shows excellent empirical performance, even for the conventional dueling bandits problem. For the case where the preference probabilities satisfy a specific type of stochastic transitivity, we provide a refined version with an improved worst case sample complexity.

## Execution Details (Getting the Code To Run)
For the sake of reproducibility, we will detail how to reproduce the results presented in the paper below: 

Run `main.r` and make sure that all packages are installed:
 
- library("parallel")  
- library(R6)
- library('sets')

