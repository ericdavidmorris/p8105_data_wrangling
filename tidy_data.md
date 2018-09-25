Tidy Data
================
Eric Morris
September 25, 2018

Gather
------

PULSE data

``` r
pulse_data = haven::read_sas("./data/public_pulse_data.sas7bdat") %>%
  janitor::clean_names()
pulse_data
```

    ## # A tibble: 1,087 x 7
    ##       id   age sex  bdi_score_bl bdi_score_01m bdi_score_06m bdi_score_12m
    ##    <dbl> <dbl> <ch>        <dbl>         <dbl>         <dbl>         <dbl>
    ##  1 10003  48.0 male            7             1             2             0
    ##  2 10015  72.5 male            6            NA            NA            NA
    ##  3 10022  58.5 male           14             3             8            NA
    ##  4 10026  72.7 male           20             6            18            16
    ##  5 10035  60.4 male            4             0             1             2
    ##  6 10050  84.7 male            2            10            12             8
    ##  7 10078  31.3 male            4             0            NA            NA
    ##  8 10088  56.9 male            5            NA             0             2
    ##  9 10091  76.0 male            0             3             4             0
    ## 10 10092  74.2 fem…           10             2            11             6
    ## # ... with 1,077 more rows

This isn'y tidy yet...

``` r
pulse_tidy = pulse_data %>% 
  gather(key = "visit", value = "bdi_score", bdi_score_bl:bdi_score_12m)
```

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

Illustrate 'separate':

``` r
pulse_tidy %>% 
  separate(visit, into = c("bdi_str", "score_str", "visit"), sep = "_") %>% 
  select(-bdi_str, -score_str) %>% 
  mutate(visit = replace(visit, visit == "bl", "00m"))
```

    ## # A tibble: 4,348 x 5
    ##       id   age sex    visit bdi_score
    ##    <dbl> <dbl> <chr>  <chr>     <dbl>
    ##  1 10003  48.0 male   00m           7
    ##  2 10015  72.5 male   00m           6
    ##  3 10022  58.5 male   00m          14
    ##  4 10026  72.7 male   00m          20
    ##  5 10035  60.4 male   00m           4
    ##  6 10050  84.7 male   00m           2
    ##  7 10078  31.3 male   00m           4
    ##  8 10088  56.9 male   00m           5
    ##  9 10091  76.0 male   00m           0
    ## 10 10092  74.2 female 00m          10
    ## # ... with 4,338 more rows

All together, the data import / cleaning pipeline is:

``` r
pulse_df = 
  haven::read_sas("./data/public_pulse_data.sas7bdat") %>%
  janitor::clean_names() %>% 
  gather(key = "visit", value = "bdi_score", bdi_score_bl:bdi_score_12m) %>% 
  separate(visit, into = c("bdi_str", "score_str", "visit"), sep = "_") %>% 
  select(-bdi_str, -score_str) %>% 
  mutate(visit = replace(visit, visit == "bl", "00m"))
```

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

Revisit FAS\_litters
--------------------

``` r
litters_data = read_csv("./data/FAS_litters.csv", col_types = "ccddiiii") %>%
   janitor::clean_names() %>%
  separate(group, into = c("dose", "day"), 3) %>% 
  mutate(dose = tolower(dose),
         wt_gain = gd18_weight - gd0_weight) %>%
  arrange(litter_number)
```

Learning Assessment, data cleaning/tidying

``` r
litters_data %>% 
  select(litter_number, ends_with("weight")) %>%  
  gather(key = gd, value = weight, gd0_weight:gd18_weight) %>% 
  mutate(gd = recode(gd, "gd0_weight" = 0, "gd18_weight" = 18)) %>% 
  arrange(litter_number)
```

    ## # A tibble: 98 x 3
    ##    litter_number      gd weight
    ##    <chr>           <dbl>  <dbl>
    ##  1 #1/2/95/2           0   27  
    ##  2 #1/2/95/2          18   42  
    ##  3 #1/5/3/83/3-3/2     0   NA  
    ##  4 #1/5/3/83/3-3/2    18   NA  
    ##  5 #1/6/2/2/95-2       0   NA  
    ##  6 #1/6/2/2/95-2      18   NA  
    ##  7 #1/82/3-2           0   NA  
    ##  8 #1/82/3-2          18   NA  
    ##  9 #100                0   20  
    ## 10 #100               18   39.2
    ## # ... with 88 more rows

Spread
------

Create 'analyis\_result'

``` r
analysis_result = tibble(
  group = c("treatment", "treatment", "placebo", "placebo"),
  time = c("pre", "post", "pre", "post"),
  mean = c(4, 8, 3.5, 4)
)
```

Make it readable (spread, the reverse of gather)

``` r
analysis_result %>% 
  spread(key = time, value = mean) %>%
  knitr::kable()
```

| group     |  post|  pre|
|:----------|-----:|----:|
| placebo   |     4|  3.5|
| treatment |     8|  4.0|
