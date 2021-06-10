# TODO

## General

## Existing functions

+ Check use of `reorder()` in `prop_ci()`.
+ Possible name change: find_similar() --> similar_cols() or compare_???
+ ??var_summary() - add na_string arg to include in missing count
+ need() could work with a vector input (multiple names). Error message can give all packages missing.
+ Review `convert_date()`. It uses `dmy()` or `dmy_hms()` as appropriate with a simplifying step. It perhaps is better broken up.

## New functions

These may already be in dev_code().

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
