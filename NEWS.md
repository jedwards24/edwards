# edwards (development version)

Minor docs fixes.

# edwards 0.3.3

* Add `dir` column to output for `dir_files()` which gives directory path for each file.  
* `var_summary()`: add `str_missing` column to output which counts matches to supplied strings.
* `var_summary()`: remove name attributes from elements in columns of output.
* `var_summary()` now uses `dplyr::n_distinct()` instead of `length(unique()` to count unique values in columns. This should only change counts for data frame columns. Previously the columns in the data frame column were compared for uniqueness, whereas now the rows are compared. For example,
  
```
x <- tibble::tibble(a = c(1:3, 1), b = c(1, 1, 1, 1))
length(unique(x))
dplyr::n_distinct(x)
```

# edwards 0.3.2

## Deprecated functions

* Renamed `count_string()` to `count_pattern()`.
* Deprecate `prop_ci()`. Replaced by `response` package [https://github.com/jedwards24/response](https://github.com/jedwards24/response).
* Deprecate `glmnet_to_table()`. See jemodel package [https://github.com/jedwards24/jemodel](https://github.com/jedwards24/jemodel).
* Deprecate `ilogit()`. Moved to jemodel package.
* Deprecate `count_matches2()`. Absorbed into rewritten `count_matches()`.

## File tools

* Add new columns to `dir_contents()` output.
* Add `recurse` argument to `dir_contents()` and `dir_size()`.
* Add `shorten` argument to `dir_contents()`.
* Add `dir_count_files()` and  `dir_count_dirs()`.
* Rewrite `latest_file()` to simplify (breaking). 

## Changes to existing functions

* Refactor `count_matches()` (tidier and faster). Absorb `count_matches2()` into `count_matches()`.
* Refactor `find_similar()` code and add tests.
* Rewrite `min_n()` and `max_n()`. Now length stable due to handling `NA`s differently (breaking).

## New functions

* Add `vcount()`. Like `count()` but for vectors.
* Add `na_if_string()` and `na_if_all()`. Used to convert elements to `NA`.

# edwards 0.3.0

## Remove Modelling Functions (Breaking Change)

Removed all deprecated modelling functions (ones starting `rang_`, `glmnet_`, or `roc_`). All are
available in the jemodel package. A simplified version of `glmnet_to_table()` has been kept in the package because I have used this most often out of these functions. 

## Remove Deprecated Functions (Breaking Change)

Removed previously deprecated functions: `count_at()`, `count_nas2()`, and `bin_numeric()`. They are replaced respectively by `count_over()`, `var_summary()`, and `bin_integer()`.

## CMD Check and Tests

* Handle NSE variables so that R CMD check passes.
* Use testthat 3rd edition and ammend tests to handle messages appropriately.

# edwards 0.2.0

## Modelling Functions

I will be moving functions for modelling to a new package jemodel. To start this:

* Deleted modelling development code (moved to jemodel).
* Deprecate all glmnet, ranger, and roc functions (everything in `glmnet.R`, `ranger.R`, and `roc.R`).

## Other

* Add NEWS file.
* Add package-edwards.R for imports. 
