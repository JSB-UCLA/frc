
# frc

<!-- badges: start -->
<!-- badges: end -->

This package frc provides functions to calculate the sample-level classical criterion (s-CC) and the sample-level Neyman-Pearson criterion (s-NPC) as described in "A flexible model-free prediction-based framework for feature ranking" by Li, Chen, and Tong (https://arxiv.org/abs/1903.05262).

## Installation

You can install the released version of frc from GitHub with:

``` r
library(devtools)
devtools::install_github("SticsRPacks/sandbox")
```

## Example

This is a basic example which shows you how to calculate s-CC and s-NPC:

``` r
library(frc)
data(X, y)
```

## s-CC calculation
```{r}
# calculate s-CC for the first feature
sCC(X[,1], y)
# calculate s-CC for all features
apply(X, 2, FUN=function(x) sCC(x, y))
```

## s-NPC calculation
```{r}
# calculate s-NPC (default: alpha = 0.3, delta = 0.05) for the first feature
sNPC(X[,1], y)
# calculate s-NPC (alpha = 0.05) for the first feature
sNPC(X[,1], y, alpha=0.05)
# calculate s-NPC, with class 1 as the more important class, for the first feature
sNPC(X[,1], y, imp_class=1)
# calculate s-NPC (default: alpha = 0.3, delta = 0.05) for all features
apply(X, 2, FUN=function(x) sNPC(x, y))
```
