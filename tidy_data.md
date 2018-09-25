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

Binding rows
------------

``` r
fellowship_ring = readxl::read_excel("./data/LotR_Words.xlsx", range = "B3:D6") %>%
  mutate(movie = "fellowship_ring")

two_towers = readxl::read_excel("./data/LotR_Words.xlsx", range = "F3:H6") %>%
  mutate(movie = "two_towers")

return_king = readxl::read_excel("./data/LotR_Words.xlsx", range = "J3:L6") %>%
  mutate(movie = "return_king")
```

Create final LOTR data

``` r
lotr_tidy = bind_rows(fellowship_ring, two_towers, return_king) %>% 
  janitor::clean_names() %>% 
  gather(key = sex, value = word, female:male) %>% 
  mutate(race = tolower(race))
```

Join datasets
-------------

Load the FAS datasets and create a joined dataset via a left join

``` r
pup_data = read_csv("./data/FAS_pups.csv", col_types = "ciiiii") %>%
  janitor::clean_names() %>%
  mutate(sex = recode(sex, `1` = "male", `2` = "female")) 

litter_data = read_csv("./data/FAS_litters.csv", col_types = "ccddiiii") %>%
  janitor::clean_names() %>%
  select(-pups_survive) %>%
  mutate(
    wt_gain = gd18_weight - gd0_weight,
    group = tolower(group))

FAS_data = left_join(pup_data, litter_data, by = "litter_number")
```

Learning assessment:

``` r
surv_os = read_csv("./data/survey_results/surv_os.csv") %>% 
  janitor::clean_names() %>% 
  rename(id = what_is_your_uni, os = what_operating_system_do_you_use)
```

    ## Parsed with column specification:
    ## cols(
    ##   `What is your UNI?` = col_character(),
    ##   `What operating system do you use?` = col_character()
    ## )

``` r
surv_pr_git = read_csv("data/survey_results/surv_program_git.csv") %>% 
  janitor::clean_names() %>% 
  rename(id = what_is_your_uni, 
         prog = what_is_your_degree_program,
         git_exp = which_most_accurately_describes_your_experience_with_git)
```

    ## Parsed with column specification:
    ## cols(
    ##   `What is your UNI?` = col_character(),
    ##   `What is your degree program?` = col_character(),
    ##   `Which most accurately describes your experience with Git?` = col_character()
    ## )

``` r
left_join(surv_os, surv_pr_git)
```

    ## Joining, by = "id"

    ## # A tibble: 175 x 4
    ##    id        os       prog  git_exp                                       
    ##    <chr>     <chr>    <chr> <chr>                                         
    ##  1 student_… <NA>     MS    Pretty smooth: needed some work to connect Gi…
    ##  2 student_… Windows… Other Pretty smooth: needed some work to connect Gi…
    ##  3 student_… Mac OS X MPH   Smooth: installation and connection with GitH…
    ##  4 student_… Windows… MS    Smooth: installation and connection with GitH…
    ##  5 student_… Mac OS X MS    Smooth: installation and connection with GitH…
    ##  6 student_… Mac OS X MS    Smooth: installation and connection with GitH…
    ##  7 student_… Windows… MPH   Pretty smooth: needed some work to connect Gi…
    ##  8 student_… Windows… MPH   Pretty smooth: needed some work to connect Gi…
    ##  9 student_… Windows… MPH   Pretty smooth: needed some work to connect Gi…
    ## 10 student_… Mac OS X <NA>  <NA>                                          
    ## # ... with 165 more rows

``` r
inner_join(surv_os, surv_pr_git)
```

    ## Joining, by = "id"

    ## # A tibble: 129 x 4
    ##    id        os       prog  git_exp                                       
    ##    <chr>     <chr>    <chr> <chr>                                         
    ##  1 student_… <NA>     MS    Pretty smooth: needed some work to connect Gi…
    ##  2 student_… Windows… Other Pretty smooth: needed some work to connect Gi…
    ##  3 student_… Mac OS X MPH   Smooth: installation and connection with GitH…
    ##  4 student_… Windows… MS    Smooth: installation and connection with GitH…
    ##  5 student_… Mac OS X MS    Smooth: installation and connection with GitH…
    ##  6 student_… Mac OS X MS    Smooth: installation and connection with GitH…
    ##  7 student_… Windows… MPH   Pretty smooth: needed some work to connect Gi…
    ##  8 student_… Windows… MPH   Pretty smooth: needed some work to connect Gi…
    ##  9 student_… Windows… MPH   Pretty smooth: needed some work to connect Gi…
    ## 10 student_… Windows… MPH   Smooth: installation and connection with GitH…
    ## # ... with 119 more rows

``` r
anti_join(surv_os, surv_pr_git)
```

    ## Joining, by = "id"

    ## # A tibble: 46 x 2
    ##    id          os                                     
    ##    <chr>       <chr>                                  
    ##  1 student_86  Mac OS X                               
    ##  2 student_91  Windows 10                             
    ##  3 student_24  Mac OS X                               
    ##  4 student_103 Mac OS X                               
    ##  5 student_163 Mac OS X                               
    ##  6 student_68  Other (Linux, Windows, 95, TI-89+, etc)
    ##  7 student_158 Mac OS X                               
    ##  8 student_19  Windows 10                             
    ##  9 student_43  Mac OS X                               
    ## 10 student_78  Mac OS X                               
    ## # ... with 36 more rows

``` r
anti_join(surv_pr_git, surv_os)
```

    ## Joining, by = "id"

    ## # A tibble: 15 x 3
    ##    id        prog  git_exp                                                
    ##    <chr>     <chr> <chr>                                                  
    ##  1 <NA>      MPH   Pretty smooth: needed some work to connect Git, GitHub…
    ##  2 student_… PhD   Pretty smooth: needed some work to connect Git, GitHub…
    ##  3 <NA>      MPH   Pretty smooth: needed some work to connect Git, GitHub…
    ##  4 <NA>      MPH   Pretty smooth: needed some work to connect Git, GitHub…
    ##  5 <NA>      MS    Pretty smooth: needed some work to connect Git, GitHub…
    ##  6 student_… MS    Pretty smooth: needed some work to connect Git, GitHub…
    ##  7 <NA>      MS    Smooth: installation and connection with GitHub was ea…
    ##  8 student_… PhD   Pretty smooth: needed some work to connect Git, GitHub…
    ##  9 student_… MPH   Smooth: installation and connection with GitHub was ea…
    ## 10 student_… MS    Smooth: installation and connection with GitHub was ea…
    ## 11 <NA>      MS    Pretty smooth: needed some work to connect Git, GitHub…
    ## 12 <NA>      MS    "What's \"Git\" ...?"                                  
    ## 13 <NA>      MS    Smooth: installation and connection with GitHub was ea…
    ## 14 <NA>      MPH   Pretty smooth: needed some work to connect Git, GitHub…
    ## 15 <NA>      MS    Pretty smooth: needed some work to connect Git, GitHub…
