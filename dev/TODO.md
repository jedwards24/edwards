# TODO

## General

+ I can't find how to import '.' so have removed it from code instead.
  - I have seen it added as `globalVariables(".")` in tidyr in the tidyr.R script.
+ Check use of `reorder()` in `prop_ci()`.
+ NEWS file and version increment.
+ Move model funcs to jemodel. Sort checking when I do this and add NEWS.
* Add a package script for imports.

## Existing functions

+ Possible name change: find_similar() --> similar_cols() or compare_???
+ ??var_summary() - add na_string arg to include in missing count
+ glmnet tools (not working correctly). Add tests.
+ need() could work with a vector input (multiple names). Error message can give all packages missing.

## New functions

These may already be in dev_code().

+ pdp functions (already written but not in package)
+ key function
+ benchmarking over arguments
+ ??Add count_sheets() - see func google doc. Used in treat....Prob won't do this unless I use it anywhere else
+ calibration plot for binary responses - split preds into deciles and get mean response for each. Plot & table.
+ simplify_datetime()
+ variable checking functions:
    + check all dates for simple datetimes and 1899/1900 dates.
    + possible dates as characters
    + count prop integers in numeric cols.
    + single value or single & NA.
    + Binary value columns.
+ Non-missing head function.
+ Convert Excel dates from char or numeric: as.Date(as.numeric(vec), origin = "1899-12-30")
+ lengths <- function(x) vapply(x, length, integer(1))

## Package check issues

+ bindings for df cols. Need to use .data$ for these
+ Testthat 3.0? https://testthat.r-lib.org/articles/third-edition.html
