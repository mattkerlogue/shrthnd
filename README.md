
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shrthnd <img src="man/figures/shrthnd_hex.png" align="right" alt="tidyods package logo" width="120" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/mattkerlogue/shrthnd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mattkerlogue/shrthnd/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Data is often published with shorthand and symbols, and regularly these
markers are found in the same container as the value. The aim of
`{shrthnd}` is to process character vectors of numerical data that
contain non-numeric shorthand and symbols.

## Installation

You can install the development version of shrthnd like so:

``` r
# install.packages("remotes")
remotes::install_github("mattkerlogue/shrthnd")
```

## Example

``` r
library(shrthnd)
x <- c("12", "34.567", "[c]", "NA", "56.78[e]", "78.9", "90.123[e]")
sh_x <- shrthnd_num(x, shorthand = c("[c]", "[e]"), na_values = "NA", digits = 2)
shrthnd_list(sh_x)
#> <shrthnd_list[2]> 
#> [c] (1 location): 3 
#> [e] (2 locations): 5, 7
tibble::tibble(sh_x = sh_x, as_num = as.numeric(sh_x), 
               as_char = as.character(sh_x), tag = shrthnd_tags(sh_x), 
               as_shrthnd = as_shrthnd(sh_x), 
               as_shrthnd2 = as_shrthnd(sh_x, digits = FALSE))
#> # A tibble: 7 × 6
#>       sh_x as_num as_char tag   as_shrthnd as_shrthnd2
#>   <sh_dbl>  <dbl> <chr>   <chr> <chr>      <chr>      
#> 1    12.00   12   12      <NA>  12.00      12         
#> 2    34.57   34.6 34.567  <NA>  34.57      34         
#> 3      [c]   NA   <NA>    [c]   [c]        [c]        
#> 4       NA   NA   <NA>    <NA>  <NA>       <NA>       
#> 5 56.78[e]   56.8 56.78   [e]   56.78[e]   56[e]      
#> 6    78.90   78.9 78.9    <NA>  78.90      78         
#> 7 90.12[e]   90.1 90.123  [e]   90.12[e]   90[e]
```

## Logo

The shrthnd logo is a combination of the word “shorthand” written in
[Pitman shorthand](https://en.wikipedia.org/wiki/Pitman_shorthand) and
an asterisk.
