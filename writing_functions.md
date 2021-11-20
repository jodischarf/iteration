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

    ##  [1]  0.2457443 -0.9542996 -0.4990034 -0.4709479  2.4366461 -0.2880790
    ##  [7] -0.6344217  0.3754319 -0.9112869  0.5406349 -0.7307414  0.4140515
    ## [13] -0.9617279  1.1953638 -0.1541838 -1.1047460  0.5093325 -2.0947372
    ## [19]  0.1849418  0.2783162  1.3239982 -0.4415491  1.8980586 -0.4293865
    ## [25]  0.2725906

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1]  0.2457443 -0.9542996 -0.4990034 -0.4709479  2.4366461 -0.2880790
    ##  [7] -0.6344217  0.3754319 -0.9112869  0.5406349 -0.7307414  0.4140515
    ## [13] -0.9617279  1.1953638 -0.1541838 -1.1047460  0.5093325 -2.0947372
    ## [19]  0.1849418  0.2783162  1.3239982 -0.4415491  1.8980586 -0.4293865
    ## [25]  0.2725906

``` r
y_vec = rnorm(40, mean = 12, sd = .3)
z_scores(x = y_vec)
```

    ##  [1] -0.85726311 -0.70806008 -1.45003442 -0.22230042 -0.48100091 -0.86798493
    ##  [7]  0.68913876  0.31572621 -1.68150658 -0.74839088 -0.01874610 -0.20544679
    ## [13]  0.89900471  0.51159520  0.62041269 -1.13229563  0.04842276  0.14284528
    ## [19]  0.04295769  0.42737033 -0.34419852  1.62327967  0.57171019  2.44605873
    ## [25]  0.09238768 -1.67638073 -0.61116188  0.05836549 -0.31036625  0.10312338
    ## [31]  1.54479675  1.50765046 -1.62166479 -0.69930763 -0.96404273 -0.44924636
    ## [37]  0.40566972  1.14807144 -0.34089146  2.19170303

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

    ##  [1] -0.85726311 -0.70806008 -1.45003442 -0.22230042 -0.48100091 -0.86798493
    ##  [7]  0.68913876  0.31572621 -1.68150658 -0.74839088 -0.01874610 -0.20544679
    ## [13]  0.89900471  0.51159520  0.62041269 -1.13229563  0.04842276  0.14284528
    ## [19]  0.04295769  0.42737033 -0.34419852  1.62327967  0.57171019  2.44605873
    ## [25]  0.09238768 -1.67638073 -0.61116188  0.05836549 -0.31036625  0.10312338
    ## [31]  1.54479675  1.50765046 -1.62166479 -0.69930763 -0.96404273 -0.44924636
    ## [37]  0.40566972  1.14807144 -0.34089146  2.19170303

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
    ## 1  5.64  3.51

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.339

## Different sample sizes, means, sds

``` r
sim_data = 
  tibble(
    x = rnorm(30, mean = 2, sd = 3)
  )
sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.32  3.10

Let’s write a function that simulates data, computes the mean and sd.

``` r
sim_mean_sd = function(n, mu = 3, sigma = 4) {
  
  # do checks on inputs
  
  sim_data = 
    tibble(
      x = rnorm(n, mean = mu, sd = sigma)
    )
  
  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
  
}
sim_mean_sd(n = 30, sigma = 3, mu = 40)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  41.3  3.27

``` r
sim_mean_sd(30)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.74  4.18
