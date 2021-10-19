Week3_Homework
================

## Task 1

\*load to_sort_pop_1.csv and to_sort_pop_1.csv from bioinformatics_data
on github.

``` r
##load vroom
library(vroom)
##read in the data files
sort_pop_1 <- vroom("C:/Users/bw21167/Desktop/bioinformatics/Data/to_sort_pop_1.csv")
```

    ## Rows: 30 Columns: 29

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: "\t"
    ## chr  (4): species, primary_threat, secondary_threat, tertiary_threat
    ## dbl (24): pop_1_2003-01-01, pop_1_2004-01-01, pop_1_2005-01-01, pop_1_2006-0...
    ## lgl  (1): pop_1_1995-01-01

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
sort_pop_2 <- vroom("C:/Users/bw21167/Desktop/bioinformatics/Data/to_sort_pop_2.csv")
```

    ## Rows: 30 Columns: 28

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: "\t"
    ## chr  (4): species, primary_threat, secondary_threat, tertiary_threat
    ## dbl (21): pop_2_2000-01-01, pop_2_2001-01-01, pop_2_2002-01-01, pop_2_2003-0...
    ## lgl  (3): pop_2_1996-01-01, pop_2_1997-01-01, pop_2_1998-01-01

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Task 2

\*these two data are shown as below:

``` r
sort_pop_1
```

    ## # A tibble: 30 x 29
    ##    species     primary_threat  secondary_threat tertiary_threat `pop_1_2003-01-~
    ##    <chr>       <chr>           <chr>            <chr>                      <dbl>
    ##  1 Schistidiu~ Habitat destru~ <NA>             <NA>                          NA
    ##  2 Paraleucob~ Exploitation    Habitat loss     <NA>                          NA
    ##  3 Scapania p~ Climate change  <NA>             <NA>                          NA
    ##  4 Seligera r~ Exploitation    <NA>             <NA>                          NA
    ##  5 Tortula su~ Habitat loss    Pollution        Climate change                96
    ##  6 Pohlia mel~ <NA>            <NA>             <NA>                         288
    ##  7 Bryum weig~ Exploitation    <NA>             <NA>                          81
    ##  8 Ceratodon ~ <NA>            <NA>             <NA>                          NA
    ##  9 Trichocole~ Climate change  <NA>             <NA>                         245
    ## 10 Bryum klin~ Habitat loss    <NA>             <NA>                          22
    ## # ... with 20 more rows, and 24 more variables: pop_1_2004-01-01 <dbl>,
    ## #   pop_1_2005-01-01 <dbl>, pop_1_2006-01-01 <dbl>, pop_1_2007-01-01 <dbl>,
    ## #   pop_1_2008-01-01 <dbl>, pop_1_2009-01-01 <dbl>, pop_1_2010-01-01 <dbl>,
    ## #   pop_1_2011-01-01 <dbl>, pop_1_2012-01-01 <dbl>, pop_1_2013-01-01 <dbl>,
    ## #   pop_1_2014-01-01 <dbl>, pop_1_2015-01-01 <dbl>, pop_1_2016-01-01 <dbl>,
    ## #   pop_1_2017-01-01 <dbl>, pop_1_2018-01-01 <dbl>, pop_1_2019-01-01 <dbl>,
    ## #   pop_1_2000-01-01 <dbl>, pop_1_2001-01-01 <dbl>, pop_1_2002-01-01 <dbl>, ...

``` r
sort_pop_2
```

    ## # A tibble: 30 x 28
    ##    species     primary_threat  secondary_threat tertiary_threat `pop_2_2000-01-~
    ##    <chr>       <chr>           <chr>            <chr>                      <dbl>
    ##  1 Sphagnum p~ <NA>            <NA>             <NA>                          NA
    ##  2 Pohlia wah~ Habitat loss    Pollution        <NA>                          NA
    ##  3 Sphagnum l~ Pollution       Exploitation     <NA>                          NA
    ##  4 Marchantia~ <NA>            <NA>             <NA>                          NA
    ##  5 Platyhypni~ Habitat loss    <NA>             <NA>                          NA
    ##  6 Scleropodi~ Pollution       <NA>             <NA>                          NA
    ##  7 Hedwigia s~ Habitat fragme~ <NA>             <NA>                          NA
    ##  8 Cephalozia~ Habitat fragme~ <NA>             <NA>                          NA
    ##  9 Rhynchoste~ <NA>            <NA>             <NA>                          NA
    ## 10 Pseudolesk~ Habitat loss    <NA>             <NA>                          70
    ## # ... with 20 more rows, and 23 more variables: pop_2_2001-01-01 <dbl>,
    ## #   pop_2_2002-01-01 <dbl>, pop_2_2003-01-01 <dbl>, pop_2_2004-01-01 <dbl>,
    ## #   pop_2_2005-01-01 <dbl>, pop_2_2006-01-01 <dbl>, pop_2_2007-01-01 <dbl>,
    ## #   pop_2_2008-01-01 <dbl>, pop_2_2009-01-01 <dbl>, pop_2_2010-01-01 <dbl>,
    ## #   pop_2_2011-01-01 <dbl>, pop_2_2012-01-01 <dbl>, pop_2_2013-01-01 <dbl>,
    ## #   pop_2_2014-01-01 <dbl>, pop_2_2015-01-01 <dbl>, pop_2_2016-01-01 <dbl>,
    ## #   pop_2_2017-01-01 <dbl>, pop_2_2018-01-01 <dbl>, pop_2_2019-01-01 <dbl>, ...

The first four columns should be self explanatory (the species binomial,
and the primary, secondary, and tertiary threat each species is being
threatened by). The rest of the columns specify the population counts
for given dates.

## Task 3

\*using tidyverse join both of these data together into a single tibble

``` r
##load the tidyverse
library("tidyverse")
```

    ## Registered S3 methods overwritten by 'readr':
    ##   method                    from 
    ##   as.data.frame.spec_tbl_df vroom
    ##   as_tibble.spec_tbl_df     vroom
    ##   format.col_spec           vroom
    ##   print.col_spec            vroom
    ##   print.collector           vroom
    ##   print.date_names          vroom
    ##   print.locale              vroom
    ##   str.col_spec              vroom

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.5     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.0.2     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x readr::col_character()  masks vroom::col_character()
    ## x readr::col_date()       masks vroom::col_date()
    ## x readr::col_datetime()   masks vroom::col_datetime()
    ## x readr::col_double()     masks vroom::col_double()
    ## x readr::col_factor()     masks vroom::col_factor()
    ## x readr::col_guess()      masks vroom::col_guess()
    ## x readr::col_integer()    masks vroom::col_integer()
    ## x readr::col_logical()    masks vroom::col_logical()
    ## x readr::col_number()     masks vroom::col_number()
    ## x readr::col_skip()       masks vroom::col_skip()
    ## x readr::col_time()       masks vroom::col_time()
    ## x readr::cols()           masks vroom::cols()
    ## x readr::default_locale() masks vroom::default_locale()
    ## x dplyr::filter()         masks stats::filter()
    ## x readr::fwf_cols()       masks vroom::fwf_cols()
    ## x readr::fwf_empty()      masks vroom::fwf_empty()
    ## x readr::fwf_positions()  masks vroom::fwf_positions()
    ## x readr::fwf_widths()     masks vroom::fwf_widths()
    ## x dplyr::lag()            masks stats::lag()
    ## x readr::locale()         masks vroom::locale()
    ## x readr::output_column()  masks vroom::output_column()
    ## x readr::problems()       masks vroom::problems()

``` r
##use full_join to join these data into a single tibble as sort_pop
sort_pop <- full_join(sort_pop_1, sort_pop_2)
```

    ## Joining, by = c("species", "primary_threat", "secondary_threat", "tertiary_threat")

``` r
##look at sort_pop
sort_pop
```

    ## # A tibble: 60 x 53
    ##    species     primary_threat  secondary_threat tertiary_threat `pop_1_2003-01-~
    ##    <chr>       <chr>           <chr>            <chr>                      <dbl>
    ##  1 Schistidiu~ Habitat destru~ <NA>             <NA>                          NA
    ##  2 Paraleucob~ Exploitation    Habitat loss     <NA>                          NA
    ##  3 Scapania p~ Climate change  <NA>             <NA>                          NA
    ##  4 Seligera r~ Exploitation    <NA>             <NA>                          NA
    ##  5 Tortula su~ Habitat loss    Pollution        Climate change                96
    ##  6 Pohlia mel~ <NA>            <NA>             <NA>                         288
    ##  7 Bryum weig~ Exploitation    <NA>             <NA>                          81
    ##  8 Ceratodon ~ <NA>            <NA>             <NA>                          NA
    ##  9 Trichocole~ Climate change  <NA>             <NA>                         245
    ## 10 Bryum klin~ Habitat loss    <NA>             <NA>                          22
    ## # ... with 50 more rows, and 48 more variables: pop_1_2004-01-01 <dbl>,
    ## #   pop_1_2005-01-01 <dbl>, pop_1_2006-01-01 <dbl>, pop_1_2007-01-01 <dbl>,
    ## #   pop_1_2008-01-01 <dbl>, pop_1_2009-01-01 <dbl>, pop_1_2010-01-01 <dbl>,
    ## #   pop_1_2011-01-01 <dbl>, pop_1_2012-01-01 <dbl>, pop_1_2013-01-01 <dbl>,
    ## #   pop_1_2014-01-01 <dbl>, pop_1_2015-01-01 <dbl>, pop_1_2016-01-01 <dbl>,
    ## #   pop_1_2017-01-01 <dbl>, pop_1_2018-01-01 <dbl>, pop_1_2019-01-01 <dbl>,
    ## #   pop_1_2000-01-01 <dbl>, pop_1_2001-01-01 <dbl>, pop_1_2002-01-01 <dbl>, ...

## Task 4

\*reshape them from wide to long format

``` r
##our data frame
sort_pop_long <- sort_pop %>%
  ##and then apply this function to shape data from wide to long format
  ##keep the four columns: species; primary_threat; secondary_threat; tertiary_threat
  ##and add population, date and abundance
  pivot_longer(cols = -c(species:tertiary_threat),
               names_to = c("population", "date"),
               names_pattern = "(.*)_(.*)",
               values_drop_na = F,
               values_to = "abundance")
##look at the long format data
sort_pop_long
```

    ## # A tibble: 2,940 x 7
    ##    species    primary_threat  secondary_threat tertiary_threat population date  
    ##    <chr>      <chr>           <chr>            <chr>           <chr>      <chr> 
    ##  1 Schistidi~ Habitat destru~ <NA>             <NA>            pop_1      2003-~
    ##  2 Schistidi~ Habitat destru~ <NA>             <NA>            pop_1      2004-~
    ##  3 Schistidi~ Habitat destru~ <NA>             <NA>            pop_1      2005-~
    ##  4 Schistidi~ Habitat destru~ <NA>             <NA>            pop_1      2006-~
    ##  5 Schistidi~ Habitat destru~ <NA>             <NA>            pop_1      2007-~
    ##  6 Schistidi~ Habitat destru~ <NA>             <NA>            pop_1      2008-~
    ##  7 Schistidi~ Habitat destru~ <NA>             <NA>            pop_1      2009-~
    ##  8 Schistidi~ Habitat destru~ <NA>             <NA>            pop_1      2010-~
    ##  9 Schistidi~ Habitat destru~ <NA>             <NA>            pop_1      2011-~
    ## 10 Schistidi~ Habitat destru~ <NA>             <NA>            pop_1      2012-~
    ## # ... with 2,930 more rows, and 1 more variable: abundance <dbl>
