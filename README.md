
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shrthnd <img src="man/figures/shrthnd_hex.png" align="right" alt="tidyods package logo" width="120" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/mattkerlogue/shrthnd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mattkerlogue/shrthnd/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Data is often published with shorthand and symbols, and regularly these
tags are found in the same container as the value. The aim of
`{shrthnd}` is to process character vectors of numerical data that
contain non-numeric shorthand and symbols.

The most common approach in data processing is to scrub the vector of
non-numeric characters so that it can be coerced to a numeric vector.
However, these tags often convey useful information which you may wish
to retain. The `shrthnd_num()` data type builds on
[`vctrs::new_rcrd()`](https://vctrs.r-lib.org/reference/new_rcrd.html)
to separate the numeric and non-numeric components while keeping them
attached to each other. This means that you can perform numeric
operations on the numeric aspects of the vector while retaining the
shorthand and symbols associated with the base data.

In addition to tags within spreadsheet and table columns, other
annotation information might be included within the sheet/table such as
in header or footer rows. `{shrthnd}` also provides methods for
processing these annotations, or to add your own annotations to an R
data frame.

## Installation

You can install the development version of shrthnd like so:

``` r
# install.packages("remotes")
remotes::install_github("mattkerlogue/shrthnd")
```

## Usage

Use `shrthnd_num()` to convert a character vector to a `shrthnd_num`
vector, either a `shrthnd_integer` or a `shrthnd_double` based on the
properties of the underlying numerical content. This is a
`vctrs::vec_rcrd()` data type that splits the numeric content and
non-numeric tags of the original character vector but keeps them
together rather than discards them.

``` r
library(shrthnd)

x <- c("12", "34.567", "[c]", "NA", "56.78 [e]", "78.9", "90.123[e]", 
       "321.09*", "987.564 \u2021", ".", "..")

sh_x <- shrthnd_num(x)

sh_x
#> <shrthnd_num[11]>
#>  [1] 12.00    34.57    [c]      <NA>     56.78[e] 78.90    90.12[e] 321.09* 
#>  [9] 987.56‡  .        ..

shrthnd_list(sh_x)
#> <shrthnd_list[6]>
#> [c] (1 location): 3 
#> [e] (2 locations): 5, 7 
#> * (1 location): 8 
#> ‡ (1 location): 9 
#> . (1 location): 10 
#> .. (1 location): 11

tbl <- tibble::tibble(
  x = x,
  sh_x = sh_x,
  as_num = as.numeric(sh_x), 
  as_char = as.character(sh_x),
  tag = shrthnd_tags(sh_x), 
  as_shrthnd = as_shrthnd(sh_x), 
  as_shrthnd2 = as_shrthnd(sh_x, digits = 3)
)

tbl
#> # A tibble: 11 × 7
#>    x             sh_x as_num as_char tag   as_shrthnd as_shrthnd2
#>    <chr>     <sh_dbl>  <dbl> <chr>   <chr> <chr>      <chr>      
#>  1 12           12.00   12   12      <NA>  12.00      12.000     
#>  2 34.567       34.57   34.6 34.567  <NA>  34.57      34.567     
#>  3 [c]            [c]   NA   <NA>    [c]   [c]        [c]        
#>  4 NA              NA   NA   <NA>    <NA>  <NA>       <NA>       
#>  5 56.78 [e] 56.78[e]   56.8 56.78   [e]   56.78[e]   56.780[e]  
#>  6 78.9         78.90   78.9 78.9    <NA>  78.90      78.900     
#>  7 90.123[e] 90.12[e]   90.1 90.123  [e]   90.12[e]   90.123[e]  
#>  8 321.09*    321.09*  321.  321.09  *     321.09*    321.090*   
#>  9 987.564 ‡  987.56‡  988.  987.564 ‡     987.56‡    987.564‡   
#> 10 .                .   NA   <NA>    .     .          .          
#> 11 ..              ..   NA   <NA>    ..    ..         ..

sh_tbl <- shrthnd_tbl(
  tbl,
  title = "Example table",
  notes = c("Note 1", "Note 2"),
  source_note = "Shrthnd documentation, 2023"
)

sh_tbl
#> # Title:    Example table
#> # A tibble: 11 × 7
#>    x             sh_x as_num as_char tag   as_shrthnd as_shrthnd2
#>    <chr>     <sh_dbl>  <dbl> <chr>   <chr> <chr>      <chr>      
#>  1 12           12.00   12   12      <NA>  12.00      12.000     
#>  2 34.567       34.57   34.6 34.567  <NA>  34.57      34.567     
#>  3 [c]            [c]   NA   <NA>    [c]   [c]        [c]        
#>  4 NA              NA   NA   <NA>    <NA>  <NA>       <NA>       
#>  5 56.78 [e] 56.78[e]   56.8 56.78   [e]   56.78[e]   56.780[e]  
#>  6 78.9         78.90   78.9 78.9    <NA>  78.90      78.900     
#>  7 90.123[e] 90.12[e]   90.1 90.123  [e]   90.12[e]   90.123[e]  
#>  8 321.09*    321.09*  321.  321.09  *     321.09*    321.090*   
#>  9 987.564 ‡  987.56‡  988.  987.564 ‡     987.56‡    987.564‡   
#> 10 .                .   NA   <NA>    .     .          .          
#> 11 ..              ..   NA   <NA>    ..    ..         ..         
#> # ☰ Source: Shrthnd documentation, 2023
#> # ☰ There are 2 notes, use `annotations(x)` to view

annotations(sh_tbl)
#> ── Notes for `sh_tbl` ──────────────────────────────────────────────────────────
#> Title: Example table
#> Source: Shrthnd documentation, 2023
#> Notes:
#> • Note 1
#> • Note 2
```

## Logo

The `{shrthnd}` package logo is a combination of the word “shorthand”
written in [Pitman
shorthand](https://en.wikipedia.org/wiki/Pitman_shorthand) alongside an
asterisk. The image was drawn by hand with plot points then adjusted for
plotting in `{ggplot2}`. The “shorthand” shape is based on the
representation in Arthur Reynold’s *Pitman’s English and Shorthand
Dictionary*, retrieved from the [Internet Archive on
2023-05-11](https://archive.org/details/in.ernet.dli.2015.449114/page/n641/mode/1up).
