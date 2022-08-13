# edwards

A package of miscellaneous R functions I wrote for general data science use. 

Although most functions work well and are stable, I regard these as still being in development and breaking changes are to be expected. They are intended for interactive use.

+ Counting functions. These begin with `count_` and count `NA`s, levels, matches, string patterns, or unique values by column in a data frame. `var_summary()` performs a number of these counts and puts the results in a table. `count_over()` gives a shortcut to use `dplyr::count()` over several variables.
+ Utility functions: `mode_stat()` (statistical mode), `factor_to_numeric()` (factor conversion), `ilogit()` (inverse logit function), `object_size_all()` (summary of memory used by objects), and `diff_days()` (difference in days between two dates), and `min_n()`/`max_n()`. 
+ Data cleaning helpers:`na_if_any()`/`na_if_string()` to replace NAs, `is_simple_datetime()` to simplify datetimes. 
+ Binning functions. `bin_integer()` and `bin_integer_fct()` for converting integer vectors to binned factors with appropriate level names.
+ Kable helpers: `my_kable()`, `kbl_ctable()`, and `split_kable()`.
+ Comparison functions. `compare_vars()` and `compare_vecs()` give pairwise similarity and ordering of two variables; `find_similar()` looks for matched or similar columns within a data frame or across two data frames; `compare_sets()` counts differences between two sets.
+ Directory functions: `dir_contents()`, `dir_size()`, and `dir_files()` give info on the contents of a directory.
+ Data relation tools: `fd_cols()` and `is_one2one()` to find functional dependencies and one-to-one relationships in a data frame.
+ `latest_file()` gets path of the "most recent" file in a folder based on its name.
+ `count2()` and `vcount()` adapt `dplyr::count()` to give more information or to work with atomic vector inputs.
+ Some minor helper functions to save a bit of typing: `count_n()` and `prinf()`. 

## Installation

You can install the package from github with:

```
# install.packages("devtools")
devtools::install_github("jedwards24/edwards")
```
