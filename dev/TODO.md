# TODO

## General

## Existing functions

+ count funcs 
    - just df inputs? (not list)
+ count2() - option for pct instead of prop?
+ `need()` could work with a vector input (multiple names). Error message can give all packages missing.
+ Review `convert_date()`. It uses `dmy()` or `dmy_hms()` as appropriate with a simplifying step. It perhaps is better broken up. Or deprecate and move to jemisc?
* `find_similar()` 
    - test it handles columns with class length > 1.
    - Add `key` arg when comparing 2 dfs.
    - `binary_numeric` arg - treat numerics with all vals 0/1/NA as separate class.
    - Rename to `find_similar_cols()` or `similar_cols()` or `similar_vars()`.
* `bin_integer()` 
    - Replace with bin_numeric().
    - Will `findInterval()` be faster than cut if generate my own labels?
* standard runtime tests to reduce code duplication (check_df?)
* `save_check` to work with csv etc.? 

## New functions

These may already be in dev_code().

+ key function
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
    - return named vector?
