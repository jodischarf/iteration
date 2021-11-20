Iteration and Listcols
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

## Define function

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
```

## Lists

``` r
l = 
  list(
    vec_numeric = 5:8,
    vec_logical = c(TRUE, FALSE),
    summary = summary(rnorm(1000, mean = 5, sd = 3))
  )
l[[3]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -4.049   3.009   4.837   4.913   6.915  14.220

``` r
l[["summary"]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -4.049   3.009   4.837   4.913   6.915  14.220

``` r
l$summary
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -4.049   3.009   4.837   4.913   6.915  14.220

## List of normals

``` r
list_norms = 
  list(
    a = rnorm(50, mean = 2, sd = 1),
    b = rnorm(50, mean = 5, sd = 3),
    c = rnorm(50, mean = 20, sd = 1.2),
    d = rnorm(50, mean = -12, sd = 0.5)
  )
mean_and_sd(list_norms[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.05 0.966

## for loop

Lets use a for loop to iterate over my list of normals.

``` r
output = vector("list", length = 4)

for (i in 1:4){ 
  
  output[[i]] = mean_and_sd(list_norms[[i]])
  
}
```

let’s use map instead …

``` r
output = map(list_norms, mean_and_sd)
output = map(list_norms, summary)
output = map_dbl(list_norms, median)
```

## LIST COLUMNS!!!!!

``` r
listcol_df =
  tibble(
    name = c("a", "b", "c", "d"),
    norms = list_norms
  )
listcol_df %>% 
  filter(name == "a")
```

    ## # A tibble: 1 × 2
    ##   name  norms       
    ##   <chr> <named list>
    ## 1 a     <dbl [50]>

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(norms)
```

    ## $a
    ##  [1] 2.1110262 1.0051106 2.9558819 1.3736225 3.1199066 2.5209793 0.7550777
    ##  [8] 2.8778406 2.1832031 0.4736120 0.2756626 3.1394413 0.4664578 4.0773679
    ## [15] 3.0682497 2.6081937 1.9383858 1.9685078 1.4733832 1.3476216 3.5293880
    ## [22] 1.9585789 2.0727319 1.8019733 2.2959635 1.7478543 1.4038408 0.3842226
    ## [29] 1.4908604 1.8741732 0.8286712 4.2125484 1.5443883 2.6691730 1.9257283
    ## [36] 2.0512494 2.3970317 1.4257963 1.4258271 1.3517169 2.3111245 2.0274298
    ## [43] 2.0078100 2.5603786 2.9280609 1.9957295 3.6292617 1.1553701 1.3931051
    ## [50] 4.2756443
    ## 
    ## $b
    ##  [1]  0.6994952  7.6637318  4.3549152  5.0664875 -0.4558401  7.6046023
    ##  [7]  3.3914903 11.7731035  7.9381340  5.5453262  5.3775133  7.4741664
    ## [13]  0.9377637  6.4000771  9.9953882  5.0137323  6.9599676 -1.8075541
    ## [19]  3.1900981  8.6852286  5.5313176  6.9730996  7.8589212  0.6400296
    ## [25]  8.8080748  7.4765765  7.8472185  6.7124113  7.8621340  7.3064536
    ## [31]  3.9689025  6.2756212  4.5180188  3.1026681  7.7042071  6.7649612
    ## [37]  5.2685021  7.7745414  5.3091042  3.4269797  2.1288402  6.7392096
    ## [43]  3.3023999  8.1212831  3.6084178  7.1843439  2.2068415 -1.6352726
    ## [49]  2.1008374  1.1095748
    ## 
    ## $c
    ##  [1] 20.60640 19.98169 21.83080 21.04626 21.24803 20.51663 20.08174 19.75893
    ##  [9] 19.75392 18.80621 19.76310 20.92632 21.61474 18.49991 18.67312 20.05774
    ## [17] 17.47533 18.99767 20.60451 19.06153 18.48838 21.06453 21.59337 19.19585
    ## [25] 19.05206 20.57400 19.35386 19.90092 19.77619 21.03975 20.51071 20.90812
    ## [33] 20.25250 19.78368 18.93895 19.61265 19.73886 20.66082 20.01501 20.78893
    ## [41] 19.49959 21.31306 21.52970 21.26159 18.78171 18.89246 19.22091 17.72295
    ## [49] 19.99721 20.11207
    ## 
    ## $d
    ##  [1] -12.49434 -11.88391 -11.80708 -12.29568 -12.56199 -12.19451 -12.00683
    ##  [8] -12.30188 -11.52529 -12.40064 -11.56391 -12.71325 -11.12851 -11.60281
    ## [15] -12.76107 -11.34623 -11.60571 -11.93307 -12.01717 -13.03683 -11.90308
    ## [22] -12.33055 -11.90559 -13.03487 -13.32150 -11.87194 -11.78284 -11.50562
    ## [29] -11.86360 -12.24310 -12.50554 -11.75373 -11.72532 -12.14679 -12.09186
    ## [36] -11.36788 -12.40441 -12.57267 -12.67268 -11.46240 -12.63363 -11.95287
    ## [43] -11.77928 -13.03400 -10.46513 -11.46606 -11.58250 -12.30963 -11.87357
    ## [50] -10.62799

``` r
listcol_df %>% 
  mutate(summaries = map(norms, mean_and_sd)) 
```

    ## # A tibble: 4 × 3
    ##   name  norms        summaries       
    ##   <chr> <named list> <named list>    
    ## 1 a     <dbl [50]>   <tibble [1 × 2]>
    ## 2 b     <dbl [50]>   <tibble [1 × 2]>
    ## 3 c     <dbl [50]>   <tibble [1 × 2]>
    ## 4 d     <dbl [50]>   <tibble [1 × 2]>

## Nested data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2021-10-16 07:02:37 (7.605)

    ## file min/max dates: 1869-01-01 / 2021-10-31

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2021-10-16 07:02:50 (1.697)

    ## file min/max dates: 1965-01-01 / 2020-02-29

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2021-10-16 07:02:56 (0.912)

    ## file min/max dates: 1999-09-01 / 2021-10-31

Nest data within location

``` r
weather_nested = 
  weather_df %>% 
  nest(data = date:tmin)
weather_nested %>% 
  unnest(data)
```

    ## # A tibble: 1,095 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2017-01-01     0   8.9   4.4
    ##  2 CentralPark_NY USW00094728 2017-01-02    53   5     2.8
    ##  3 CentralPark_NY USW00094728 2017-01-03   147   6.1   3.9
    ##  4 CentralPark_NY USW00094728 2017-01-04     0  11.1   1.1
    ##  5 CentralPark_NY USW00094728 2017-01-05     0   1.1  -2.7
    ##  6 CentralPark_NY USW00094728 2017-01-06    13   0.6  -3.8
    ##  7 CentralPark_NY USW00094728 2017-01-07    81  -3.2  -6.6
    ##  8 CentralPark_NY USW00094728 2017-01-08     0  -3.8  -8.8
    ##  9 CentralPark_NY USW00094728 2017-01-09     0  -4.9  -9.9
    ## 10 CentralPark_NY USW00094728 2017-01-10     0   7.8  -6  
    ## # … with 1,085 more rows

``` r
weather_lm = function(df) {
  
  lm(tmax ~ tmin, data = df)
  
}
weather_lm(weather_nested$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
map(weather_nested$data, weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

``` r
weather_nested %>% 
  mutate(lm_results = map(data, weather_lm))
```

    ## # A tibble: 3 × 4
    ##   name           id          data               lm_results
    ##   <chr>          <chr>       <list>             <list>    
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]> <lm>      
    ## 2 Waikiki_HA     USC00519397 <tibble [365 × 4]> <lm>      
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 × 4]> <lm>

## Napoleon!!!

Function to get reviews / stars

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
map(urls, get_page_reviews)
```

    ## [[1]]
    ## # A tibble: 10 × 3
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
    ## 
    ## [[2]]
    ## # A tibble: 10 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  2 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ##  3 Classic Film                                          5.0 ou… "\n  Had to or…
    ##  4 hehehehe                                              5.0 ou… "\n  goodjobbo…
    ##  5 Painful                                               1.0 ou… "\n  I think I…
    ##  6 GRAND                                                 5.0 ou… "\n  GRAND\n"  
    ##  7 Hello, 90s                                            5.0 ou… "\n  So nostal…
    ##  8 Cult Classic                                          5.0 ou… "\n  Watched i…
    ##  9 Format was inaccurate                                 4.0 ou… "\n  There was…
    ## 10 Good funny                                            3.0 ou… "\n  Would rec…
    ## 
    ## [[3]]
    ## # A tibble: 10 × 3
    ##    title                                       stars              text          
    ##    <chr>                                       <chr>              <chr>         
    ##  1 Not available w/in 48 hour window           1.0 out of 5 stars "\n  I couldn…
    ##  2 Your mom went to college.                   5.0 out of 5 stars "\n  Classic …
    ##  3 Very funny movie                            5.0 out of 5 stars "\n  I watch …
    ##  4 Watch it twice! Trust me!                   5.0 out of 5 stars "\n  Nothing …
    ##  5 A classic                                   5.0 out of 5 stars "\n  If you d…
    ##  6 Can't say how many times I've seen          5.0 out of 5 stars "\n  Such a g…
    ##  7 I pity the fool who doesn’t own this movie. 5.0 out of 5 stars "\n  I love t…
    ##  8 I don’t know why it’s so popular!           2.0 out of 5 stars "\n  My girlf…
    ##  9 Okay                                        3.0 out of 5 stars "\n  Okay\n"  
    ## 10 A WHOLESOME comedic journey                 5.0 out of 5 stars "\n  Not a mo…
    ## 
    ## [[4]]
    ## # A tibble: 10 × 3
    ##    title                                         stars              text        
    ##    <chr>                                         <chr>              <chr>       
    ##  1 Hilarious                                     5.0 out of 5 stars "\n  Funny\…
    ##  2 Love it                                       5.0 out of 5 stars "\n  What o…
    ##  3 WORTH IT!                                     5.0 out of 5 stars "\n  It's t…
    ##  4 Funny movie.                                  5.0 out of 5 stars "\n  Great …
    ##  5 Best movie ever!                              5.0 out of 5 stars "\n  Got th…
    ##  6 I was stuck in the oil patch back in the day. 5.0 out of 5 stars "\n  I watc…
    ##  7 Funny Dork humor                              5.0 out of 5 stars "\n  Humor …
    ##  8 Still funny!                                  5.0 out of 5 stars "\n  Still …
    ##  9 Love it!! 💜                                  5.0 out of 5 stars "\n  Love i…
    ## 10 LOVE it                                       5.0 out of 5 stars "\n  cult c…
    ## 
    ## [[5]]
    ## # A tibble: 10 × 3
    ##    title                           stars              text                      
    ##    <chr>                           <chr>              <chr>                     
    ##  1 Perfect                         5.0 out of 5 stars "\n  Exactly what I asked…
    ##  2 Love this movie!                5.0 out of 5 stars "\n  Great movie and sent…
    ##  3 Love it                         5.0 out of 5 stars "\n  Love this movie. How…
    ##  4 As described                    3.0 out of 5 stars "\n  Book is as described…
    ##  5 GOSH!!!                         5.0 out of 5 stars "\n  Just watch the movie…
    ##  6 Watch it right now              5.0 out of 5 stars "\n  You need to watch th…
    ##  7 At this point it’s an addiction 5.0 out of 5 stars "\n  I watch this movie w…
    ##  8 💕                              5.0 out of 5 stars "\n  Hands down, one of m…
    ##  9 Good dumb movie                 5.0 out of 5 stars "\n  I really wanted to s…
    ## 10 funny                           5.0 out of 5 stars "\n  so funny and inventi…

``` r
napoleon_df = 
  tibble(
    urls = urls
  )
napoleon_df %>% 
  mutate(reviews = map(urls, get_page_reviews)) %>% 
  select(reviews) %>% 
  unnest()
```

    ## Warning: `cols` is now required when using unnest().
    ## Please use `cols = c(reviews)`

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
