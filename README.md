# edwards

A package of miscellaneous R functions I wrote for personal use. 

Although most functions work well and are stable, I regard these as being in early development phase and breaking changes are to be expected. They are intended for interactive use.

The functions are a loose collection of mainly data science themed functions. In rough categories:

+ Counting functions. These begin with `count_` and count NAs, levels, string patterns, or unique values by variable in a data frame. `count_over()` gives a shortcut to use `dplyr::count()` over several variables. `var_summary()` performs a number of these counts and puts the results in a table.
+ Utility functions. `mode_stat()` (Statistical mode), `factor_to_numeric()` (factor conversion), `ilogit()` (inverse logit function), `object_size_all()` (summary of memory used by objects), and `min_n()`/`max_n()`.
+ Binning functions. `bin_integer()` and `bin_integer_fct()` for converting integer vectors to binned factors with appropriate level names.
+ Kable helpers. `my_kable()` and `split_kable()`.
+ Comparison functions. `compare_vars()` and `compare_vecs()` give pairwise similarity and ordering of two variables; `find_similar()` looks for matched or similar columns within a data frame or across two data frames; `is_one2one()` checks if two columns in a data frame are one-to-one; `compare_sets()` gets similarities and differences between two sets.
+ Directory functions: `dir_contents()`, `dir_size()`, and `dir_files()` give info on the contents of a directory.
+ `prop_ci()`: Quick plotting of mean and confidence intervals of binary target variable by discrete predictor variables.
+ `diff_days()`: The difference in days between two dates.
+ `latest_file()`: get path of "most recent" file in a folder with a given root name.
+ Some minor helper functions to save a bit of typing: `count_n()` and `prinf()`. 

## Installation

You can install the package from github with:

```
# install.packages("devtools")
devtools::install_github("jedwards24/edwards")
```
