# sh_dbl generation

    Code
      shrthnd_num(x)
    Output
      <shrthnd_double[7]>
      [1] 12.00     34.57     [c]       <NA>      56.78 [e] 78.90     90.12 [e]

---

    Code
      tibble::tibble(x = x, sh_x = sh_x)
    Output
      # A tibble: 7 x 2
        x              sh_x
        <chr>      <sh_dbl>
      1 12            12.00
      2 34.567        34.57
      3 [c]             [c]
      4 NA               NA
      5 56.78 [e] 56.78 [e]
      6 78.9          78.90
      7 90.123[e] 90.12 [e]

# sh_int generation

    Code
      shrthnd_num(y)
    Output
      <shrthnd_integer[7]>
      [1] 12     34     [c]    <NA>   56 [e] 78     90 [e]

---

    Code
      tibble::tibble(y = y, sh_y = sh_y)
    Output
      # A tibble: 7 x 2
        y          sh_y
        <chr>  <sh_int>
      1 12           12
      2 34           34
      3 [c]         [c]
      4 NA           NA
      5 56 [e]   56 [e]
      6 78           78
      7 90[e]    90 [e]

# shrthnd_num errors

    Code
      shrthnd_num("a")
    Message <cliMessage>
      ! `x` does not contain numeric content
    Error <rlang_error>
      unable to convert `x` to a <shrthnd_num>

---

    Code
      shrthnd_num(c("123", "567"))
    Message <cliMessage>
      ! No shorthand detected in `x`
    Error <rlang_error>
      unable to convert `x` to a <shrthnd_num>

