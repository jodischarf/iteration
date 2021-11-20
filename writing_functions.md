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

    ##  [1] -1.22601860  0.23486610  0.34160935 -0.66973870  0.75667368 -0.01950933
    ##  [7]  1.01733631 -1.57659028  0.37561820  1.15574639  1.23887961 -0.14814616
    ## [13]  0.83414603 -0.36889750 -1.08177458  1.12077158  0.53304493 -1.18328337
    ## [19]  0.55690991 -2.22153901 -0.45334829 -0.66279025 -0.51714415  1.90951587
    ## [25]  0.05366228

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1] -1.22601860  0.23486610  0.34160935 -0.66973870  0.75667368 -0.01950933
    ##  [7]  1.01733631 -1.57659028  0.37561820  1.15574639  1.23887961 -0.14814616
    ## [13]  0.83414603 -0.36889750 -1.08177458  1.12077158  0.53304493 -1.18328337
    ## [19]  0.55690991 -2.22153901 -0.45334829 -0.66279025 -0.51714415  1.90951587
    ## [25]  0.05366228

``` r
y_vec = rnorm(40, mean = 12, sd = .3)
z_scores(x = y_vec)
```

    ##  [1] -0.33678556 -0.22907938 -1.12221229 -0.78309544  0.53617776 -1.97934881
    ##  [7]  0.91622309 -1.87821636  0.27811517  0.18546743 -0.17447701  1.41676953
    ## [13] -0.91938467  0.20592072  0.79073838 -0.33614439  1.59753241 -0.54183356
    ## [19] -1.27590889  0.74776388 -0.50618816 -0.39530206 -0.94423630 -0.57593959
    ## [25]  0.09128609 -1.51492941  1.29919031  1.29880032  0.50897918 -0.14364554
    ## [31]  0.94625989 -0.52915281  1.30662524 -0.16685681  1.91048213  0.65928610
    ## [37] -1.35405847  1.66570240 -0.03577492 -0.61874957

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

    ##  [1] -0.33678556 -0.22907938 -1.12221229 -0.78309544  0.53617776 -1.97934881
    ##  [7]  0.91622309 -1.87821636  0.27811517  0.18546743 -0.17447701  1.41676953
    ## [13] -0.91938467  0.20592072  0.79073838 -0.33614439  1.59753241 -0.54183356
    ## [19] -1.27590889  0.74776388 -0.50618816 -0.39530206 -0.94423630 -0.57593959
    ## [25]  0.09128609 -1.51492941  1.29919031  1.29880032  0.50897918 -0.14364554
    ## [31]  0.94625989 -0.52915281  1.30662524 -0.16685681  1.91048213  0.65928610
    ## [37] -1.35405847  1.66570240 -0.03577492 -0.61874957

## Multiple outputs

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  output_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  
  return(output_df)
  
}
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.58  4.16

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  11.9 0.302
