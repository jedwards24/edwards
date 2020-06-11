# edwards

A package of miscellaneous R functions I wrote for personal use. 

Although most functions work well and are stable, I regard these as being in early development phase and breaking changes are to be expected.

The functions are a loose collection of mainly data science themed functions. A quick summary in rough categories is:

+ Counting functions. These begin with `count_` and count NAs, levels, string patterns, or unique values by variable in a data frame. `count_over()` gives a shortcut to use `dplyr::count()` over several variables. `var_summary()` performs a number of these counts and puts the results in a table.
+ Basic functions. `mode_stat()` (Statistical mode), `factor_to_numeric()` (factor conversion), `ilogit()` (inverse logit function), `object_size_all()` (summary of memory used by objects), and `min_n()`/`max_n()`.
+ Binning functions. `bin_integer()` and `bin_integer_fct()` for converting integer vectors to binned factors with appropriate level names.
+ Kable helpers. `my_kable()` and `split_kable()`.
+ ROC functions. `roc_plot()` and `roc_cut()` to easily get ROC plots and optimal cuts.
+ Ranger functions. Several functions beginning `rang_` to work with the ranger package: mtry tuning, ROC output, out-of-bag errors by number of trees.
+ Comparison functions. `compare_vars()` gives pairwise similarity and ordering of two variables; `find_similar()` looks for matched or similar columns within a data frame or across two data frames; `is_one2one()` checks if two columns in a data frame are one-to-one; `compare_sets()` gets similarities and differences between two sets.
+ My variants on `DataExplorer::plot_missing()`: `plot_nas()` and `plot_missing2()`.
+ `prop_ci()`: Quick plotting of mean and confidence intervals of binary target variable by discrete predictor variables.
+ `convert_date()`: date conversion for a particular pattern.
+ `latest_file()`: get path of "most recent" file in a folder with a given root name.
+ `glmnet_to_table()`: put coefficients of a glmnet model into a more convenient table.
+ Some minor helper functions to save a bit of typing: `count_n()` and `prinf()`. 

I haven't finalised dependencies yet. Some functions are tidyverse-based, while in others I have made an effort to stick to base R. The ROC and ranger functions require ROCR and ranger.

## Installation

You can install the package from github with:

```
# install.packages("devtools")
devtools::install_github("jedwards24/edwards")
```
