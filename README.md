# edwards

This is a package of some R functions I wrote for personal use.

The functions are a loose collection with a general data science theme. A quick summary in rough categories is:

+ Counting functions. These begin with `count_` and count NAs, levels, string patterns, or unique values by variable in a data frame. `count_at()` gives a shortcut to use `dplyr::count()` over several variables.
+ Basic functions. `mode_stat()` (Statistical mode), `factor_to_numeric()` (factor conversion), `ilogit()` (inverse logit function), and `object_size_all()` (summary of memory used by objects).
+ Binning functions. `bin_numeric()` and `bin_integer_fct()` for converting to binned factors with appropriate level names.
+ Kable helpers. `my_kable()` and `split_kable()`.
+ ROC functions. `roc_plot()` and `roc_cut()` to easily get ROC plots and optimal cuts.
+ Ranger functions. Several functions beginning `rang_` to work with the ranger package: mtry tuning, ROC output, out-of-bag errors by number of trees.
+ My variants on `DataExplorer::plot_missing()`: `plot_nas()` and `plot_missing2()`.
+ `prop_ci()`: Quick plotting of mean and confidence intervals of binary target variable by discrete predictor variables.
+ `compare_vars()`: Pairwise similarity and ordering of two variables.
+ `convert_date()`: date conversion for a particular pattern.

I haven't finalised dependencies yet. Some functions are tidyverse-based, while in others I have made an effort to stick to base R. The ROC and ranger functions require ROCR and ranger.
