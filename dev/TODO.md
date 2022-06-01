# TODO

## General

Update README.md.

## Existing functions

+ count funcs 
    - just df inputs? (not list)
+ count2() 
    - Option for pct instead of prop?
    - Arg for prop name.
    - Check for name clash with prop - use prop_prop/prop2?
+ `var_summary()`: 
    - add na_string arg to include in missing count (or separate column).
    - Many columns are named vectors - strip these names?
    - Check class used with multi-class cols.
+ `need()` could work with a vector input (multiple names). Error message can give all packages missing.
+ Review `convert_date()`. It uses `dmy()` or `dmy_hms()` as appropriate with a simplifying step. It perhaps is better broken up.
+ `dir_contents()` has minimal tests.
* `find_similar()` 
    - test it handles columns with class length > 1.
    - Add `key` arg when comparing 2 dfs.
    - `binary_numeric` arg - treat numerics with all vals 0/1/NA as separate class.
    - Rename to `find_similar_cols()` or `similar_cols()`.
* `bin_integer()` 
    - Replace with bin_numeric().
    - Will `findInterval()` be faster than cut if generate my own labels?
* standard runtime tests to reduce code duplication (check_df?)
* `save_check` to work with csv etc.? 
* `factor_to_numeric()` converts "TRUE"/"FALSE" levels - maybe should not do this. If it does then it should also handle "True", "T" etc. (i.e., anything that as.logical() handles).
* `latest_file()` the `ext` arg maybe should accept a dot in front. So ".csv" and "csv" behave the same.

## New functions

These may already be in dev_code().

* na_if_string()
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
