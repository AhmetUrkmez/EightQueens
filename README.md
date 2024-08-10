## Eight Queens Problem

This problem was solved using GA.

**Objective Function**

$MinZ = \sum_{j=1}^7 \sum_{k=j+1}^8 C_{jk}$ and $C_{jk} \in \{0, 1\}$

**Components of Algorithm**

+ Roulette Selection
+ Permutative Crossover
+ Permutative Mutation

**Parameters of Algorithm**

+ Population Size
+ Crossover Rate
+ Mutation Rate

**Benchmark Results**

```r
GA(50, 0.95, 0.95)
```

| Selection Type | Crossover Type | Descriptive Statistics | Time | Trials
| --- | --- | --- | --- | --- |
| Roulette | Order One | Mean: $6.84$ <br> Variance: $38.9$ <br> Max: $66$ <br> Min: $1$ | 21.4 secs | 1000 |
