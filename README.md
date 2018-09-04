# AccData

The goal of AccData is to extract information from yearly accident report datasets
such as number of occurrences and locations of accidents in each state.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
fars_read("accident_2015.csv")
fars_read_years({"2015","2014","2013"})
fars_summarize_years({"2015","2014","2013"})
fars_map_state("2","2013")
```
[![Build Status](https://travis-ci.org/yecong/accdata.svg?branch=master)](https://travis-ci.org/yecong/accdata)
