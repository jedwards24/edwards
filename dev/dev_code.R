# Incomplete code on possible updates

# keys? ---------------
# Sub function to build functional dependencies?
# is_one2one() might be useful here
# Or use chk::check_key()
library(tidyverse)

only_uniques <- function(cols, data) {
  first_dupe <- anyDuplicated(data[, cols])
  first_dupe == 0
}

dt <- mtcars
combo_list <- lapply(
  X = seq_along(dt),
  FUN = combn,
  x = names(dt),
  simplify = FALSE
)

combos <- tibble(columns = unlist(combo_list, recursive = FALSE)) %>%
  mutate(valid = vapply(columns, only_uniques, logical(1), data = dt))

filter(combos, valid == T) %>% slice(1) %>% pull(columns)

anyDuplicated(dt[c("mpg", "wt")])

head(dt)
as_tibble(dt)
dt[1:11, ]
only_uniques(c("mpg", "wt"), dt)

?anyDuplicated

# benchmarking func ---------
# I want to automate comparisons between functions with a range of n arguments, then plot the results.
library(microbenchmark)
#https://stackoverflow.com/questions/48362852/microbenchmark-pass-a-variable-to-the-function
#https://github.com/r-lib/bench

n_vec <- c(1e3, 1e4, 1e5, 1e6)
set.seed(11)
for(i in seq_along(n_vec)){
  mb <- microbenchmark(...)
}

#simplify_datetime() --------------
# Wrapper for following but print which cols are converted.
# mutate_if(dt, is_simple_datetime, ~as.Date(.))

# count_int() var_integer()? ---------
summarise_if(dt2, is.numeric, count_int) %>% pivot_longer(cols = everything())

# datetime_summary() -------
#Explore which are simple datetimes. Maybe also check which are 0 in Excel i.e.equal as.POSIXct("1899-12-31", tz = "UTC")
summarise_if(dt, is.POSIXct, is_simple_datetime) %>% pivot_longer(cols = everything())


# Check package is installed----------
# The aim here is for scripts where a function is called using `::` but I don't
# call library to avoid attaching it (to avoid conflicts etc.)
# I want to check that package is installed and indicate that it is needed.
#
# Don't use require() since that attaches it.

# options are:
#
#https://stackoverflow.com/questions/9341635/check-for-installed-packages-before-running-install-packages
nzchar(system.file(package = "lesson"))
pkg %in% rownames(installed.packages())

# From R packages book for “suggests” packages:
  if (!requireNamespace("DataExplorer", quietly = TRUE)) {
    stop("Package \"DataExplorer\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
# `requireNamespace()` loads but does not attach its target.

# Check if package is loaded:
if(!("tidyverse" %in% tolower(.packages())))
# If installed:
nzchar(system.file(package = "rpart.plot"))
library()$results[,1]
#  https://rpubs.com/Mentors_Ubiqum/list_packages
# Also:
search()

