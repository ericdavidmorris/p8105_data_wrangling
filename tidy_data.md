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
