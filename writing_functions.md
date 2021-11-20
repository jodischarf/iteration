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

    ##  [1] -0.20131795 -0.66518263 -0.82430899  1.25370674 -0.53540759  0.15893571
    ##  [7]  1.22412954 -0.62672240  0.95423588 -0.41950210  0.27937021 -0.26860363
    ## [13] -2.37064089 -1.51465527 -0.03848878 -1.61613696  1.33447587  0.07042455
    ## [19]  0.79498504  0.55727353  0.36443585  1.16902046 -0.98438277  0.57254719
    ## [25]  1.33180937

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1] -0.20131795 -0.66518263 -0.82430899  1.25370674 -0.53540759  0.15893571
    ##  [7]  1.22412954 -0.62672240  0.95423588 -0.41950210  0.27937021 -0.26860363
    ## [13] -2.37064089 -1.51465527 -0.03848878 -1.61613696  1.33447587  0.07042455
    ## [19]  0.79498504  0.55727353  0.36443585  1.16902046 -0.98438277  0.57254719
    ## [25]  1.33180937

``` r
y_vec = rnorm(40, mean = 12, sd = .3)
z_scores(x = y_vec)
```

    ##  [1] -0.56207540 -0.27386923 -0.55647342  1.25612495 -0.72511066  0.58126793
    ##  [7] -0.29755490  1.05763154 -0.08861262 -0.03028176  0.30108451  0.70419932
    ## [13] -0.77548974  0.43274495  1.27300583  0.64232218  0.56201447  0.11546752
    ## [19] -1.98953238  0.59130495 -0.84855249 -0.67581242 -0.88565386  0.20755266
    ## [25]  1.39888224 -0.24814427 -0.30627632 -1.25047548 -1.79552012  1.77149435
    ## [31] -0.24004099 -2.01768347  0.22409377  0.27294160  2.49568453  0.56402322
    ## [37] -0.21090305  1.31565642 -0.98944593 -0.99998843

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

    ##  [1] -0.56207540 -0.27386923 -0.55647342  1.25612495 -0.72511066  0.58126793
    ##  [7] -0.29755490  1.05763154 -0.08861262 -0.03028176  0.30108451  0.70419932
    ## [13] -0.77548974  0.43274495  1.27300583  0.64232218  0.56201447  0.11546752
    ## [19] -1.98953238  0.59130495 -0.84855249 -0.67581242 -0.88565386  0.20755266
    ## [25]  1.39888224 -0.24814427 -0.30627632 -1.25047548 -1.79552012  1.77149435
    ## [31] -0.24004099 -2.01768347  0.22409377  0.27294160  2.49568453  0.56402322
    ## [37] -0.21090305  1.31565642 -0.98944593 -0.99998843

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
    ## 1  7.04  4.71

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  11.9 0.315

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
    ## 1 0.594  3.20

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
    ## 1  39.0  2.60

``` r
sim_mean_sd(30)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.91  3.10

## Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"
dynamite_html = read_html(url)
review_titles = 
  dynamite_html %>%
  html_elements(".a-text-bold span") %>%
  html_text()
review_stars = 
  dynamite_html %>%
  html_elements("#cm_cr-review_list .review-rating") %>%
  html_text()
review_text = 
  dynamite_html %>%
  html_elements(".review-text-content span") %>%
  html_text()
reviews = 
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
```

Okay but there are a lot of pages of reviews.

Write a function that gets reviews based on page url

``` r
get_page_reviews = function(page_url) {
  
  page_html = read_html(page_url)
  review_titles = 
    page_html %>%
    html_elements(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    page_html %>%
    html_elements("#cm_cr-review_list .review-rating") %>%
    html_text()
  
  review_text = 
    page_html %>%
    html_elements(".review-text-content span") %>%
    html_text()
  
  reviews = 
    tibble(
      title = review_titles,
      stars = review_stars,
      text = review_text
    )
  
  return(reviews)
  
  
}
base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
urls = str_c(base_url, 1:5)
bind_rows(
  get_page_reviews(urls[1]),
  get_page_reviews(urls[2]),
  get_page_reviews(urls[3]),
  get_page_reviews(urls[4]),
  get_page_reviews(urls[5]))
```

    ## # A tibble: 50 × 3
    ##    title                            stars              text                     
    ##    <chr>                            <chr>              <chr>                    
    ##  1 no brainer                       5.0 out of 5 stars "\n  watched this with m…
    ##  2 Yeah., it was pretty good.       5.0 out of 5 stars "\n  Yeah, it was pretty…
    ##  3 Love it                          5.0 out of 5 stars "\n  Didn't like this wh…
    ##  4 it was                           5.0 out of 5 stars "\n  mad good yo\n"      
    ##  5 Fun!                             4.0 out of 5 stars "\n  Fun and entertainin…
    ##  6 Vintage                          5.0 out of 5 stars "\n  Easy to order. I th…
    ##  7 too many commercials             1.0 out of 5 stars "\n  5 minutes into the …
    ##  8 this film is so good!            5.0 out of 5 stars "\n  VOTE FOR PEDRO!\n"  
    ##  9 Good movie                       5.0 out of 5 stars "\n  Weird story, goofy …
    ## 10 I Just everyone to know this.... 5.0 out of 5 stars "\n  VOTE FOR PEDRO !!!!…
    ## # … with 40 more rows

``` r
f = function(x) {
  z = x + y
  z
}
x = 1
y = 2
f(x = y)
```

    ## [1] 4
