BOJfame
================

The `BOJfame` package provides an `R` interface to the Bank of Japan’s
[Time-Series Data
Search](https://www.stat-search.boj.or.jp/index_en.html) portal. It
allows users to download data directly into R using individual series
codes.

The package name `BOJfame` refers to the “famecgi2” script underpinning
the BOJ’s database system, likely named after the
[FAME](https://en.wikipedia.org/wiki/FAME_(database)) database tool.

## Installing the package

You can install the package directly from GitHub:

``` r
library(devtools)
install_github("stefanangrick/BOJfame")  # GitHub
```

## Example usage

To start using the package, load it into your R session.

``` r
library("BOJfame")
```

Next, use the `get_bojfame()` function to download data series. Ensure
that all series have the same frequency, or the function will return an
error. The following example retrieves two daily-frequency series for a
specified date range:

``` r
bojdata <- get_bojfame(series_codes = c("FM01'STRDCLUCON", "IR01'MADR1Z@D"),
                       start_year = 1995, end_year = 2024)
```

The `get_bojfame()` function returns a list containing two key elements.
First, a [tibble](https://tibble.tidyverse.org/) data frame with the
actual time-series data, accessible via `$data`.

``` r
bojdata$data
```

    ## # A tibble: 21,916 × 3
    ##    date       indicator       obs_value
    ##    <date>     <chr>               <dbl>
    ##  1 1995-01-01 FM01'STRDCLUCON     NA   
    ##  2 1995-01-01 IR01'MADR1Z@D        1.75
    ##  3 1995-01-02 FM01'STRDCLUCON     NA   
    ##  4 1995-01-02 IR01'MADR1Z@D        1.75
    ##  5 1995-01-03 FM01'STRDCLUCON     NA   
    ##  6 1995-01-03 IR01'MADR1Z@D        1.75
    ##  7 1995-01-04 FM01'STRDCLUCON     NA   
    ##  8 1995-01-04 IR01'MADR1Z@D        1.75
    ##  9 1995-01-05 FM01'STRDCLUCON     NA   
    ## 10 1995-01-05 IR01'MADR1Z@D        1.75
    ## # ℹ 21,906 more rows

Second, a [tibble](https://tibble.tidyverse.org/) data frame with
metadata, accessible via `$meta`.

``` r
bojdata$meta
```

    ## # A tibble: 9 × 3
    ##   Attribute            `FM01'STRDCLUCON`                         `IR01'MADR1Z@D`
    ##   <chr>                <chr>                                     <chr>          
    ## 1 Series code          FM01'STRDCLUCON                           IR01'MADR1Z@D  
    ## 2 Name of time-series  Call Rate, Uncollateralized Overnight, A… The Basic Disc…
    ## 3 Unit                 percent per annum                         Percent per an…
    ## 4 Statistical category Call Rate                                 The Basic Disc…
    ## 5 Observation          AVERAGED                                  END            
    ## 6 start of time-series 1998/01/05                                1882/10/11     
    ## 7 End of time-series   2024/12/26                                2024/12/26     
    ## 8 Frequency            DAILY                                     DAILY          
    ## 9 Notes                <NA>                                      <NA>

To plot the data with [ggplot2](https://ggplot2.tidyverse.org), run the
following:

``` r
library("dplyr")
library("ggplot2")

bojdata$data <- mutate(bojdata$data,
                       indicator = recode(indicator,
                                          "FM01'STRDCLUCON" = "Call rate",
                                          "IR01'MADR1Z@D" = "Basic loan rate"))

ggplot(bojdata$data, aes(x = date, y = obs_value)) +
  geom_line(aes(colour = indicator)) +
  labs(title = "Interest rates", x = "Date", y = "%") +
  theme(legend.title = element_blank())
```

![](README_files/figure-gfm/plot-1.png)<!-- -->

Note that BOJ data sets use various time formats. The
[zoo](https://cran.r-project.org/package=zoo) package (e.g.,
`as.yearmon()`) can handle most of these formats.

## Note

This package is neither officially related to nor endorsed by the [Bank
of Japan](https://www.boj.or.jp/). Please avoid overloading the BOJ
servers with unnecessary requests.
