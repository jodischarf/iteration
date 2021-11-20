Writing Functions
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)
theme_set(theme_minimal() + theme(legend.position = "bottom"))
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)
scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)
(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.84125474 -1.62139984 -1.16813407  1.65222659 -1.28671026  1.30612755
    ##  [7] -0.60183297 -1.23022784 -0.63403366  0.61354470 -0.63597528  0.63603163
    ## [13] -0.22861637  0.26937084  1.03949273  1.20415730 -1.29195625  0.34112793
    ## [19]  1.41624689  0.93288120  0.15638983  0.18146108  0.92184255 -1.22903428
    ## [25]  0.09827474

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1] -0.84125474 -1.62139984 -1.16813407  1.65222659 -1.28671026  1.30612755
    ##  [7] -0.60183297 -1.23022784 -0.63403366  0.61354470 -0.63597528  0.63603163
    ## [13] -0.22861637  0.26937084  1.03949273  1.20415730 -1.29195625  0.34112793
    ## [19]  1.41624689  0.93288120  0.15638983  0.18146108  0.92184255 -1.22903428
    ## [25]  0.09827474

``` r
y_vec = rnorm(40, mean = 12, sd = .3)
z_scores(x = y_vec)
```

    ##  [1]  0.784564771  0.136816526  0.164125215 -0.800540749 -0.099001274
    ##  [6] -1.097428349  1.958634489  0.673603949 -1.361593497 -0.160230792
    ## [11] -1.375540990  0.645714421  0.412305098 -0.655796460 -0.103293379
    ## [16]  0.506462312  2.405157438  1.879721634 -1.096240897 -0.042600007
    ## [21]  2.104099588 -1.384655493 -0.009351329  0.549153188  0.835444751
    ## [26]  0.779229081 -0.917193698 -0.402608874 -1.612552684  0.686847342
    ## [31] -0.679565658  0.299144219 -0.033471762 -0.106127873 -1.223525552
    ## [36]  0.251766260 -0.905807116  0.520724033 -0.440093138 -1.086294745

How great is this??

Only kinda great.

Let’s try again.

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}
```

``` r
z_scores(3)
```

    ## Error in z_scores(3): x should have at least 3 numbers

``` r
z_scores(c("my", "name", "is", "jeff"))
```

    ## Error in z_scores(c("my", "name", "is", "jeff")): x needs to be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): x needs to be numeric

``` r
z_scores(x = y_vec)
```

    ##  [1]  0.784564771  0.136816526  0.164125215 -0.800540749 -0.099001274
    ##  [6] -1.097428349  1.958634489  0.673603949 -1.361593497 -0.160230792
    ## [11] -1.375540990  0.645714421  0.412305098 -0.655796460 -0.103293379
    ## [16]  0.506462312  2.405157438  1.879721634 -1.096240897 -0.042600007
    ## [21]  2.104099588 -1.384655493 -0.009351329  0.549153188  0.835444751
    ## [26]  0.779229081 -0.917193698 -0.402608874 -1.612552684  0.686847342
    ## [31] -0.679565658  0.299144219 -0.033471762 -0.106127873 -1.223525552
    ## [36]  0.251766260 -0.905807116  0.520724033 -0.440093138 -1.086294745
