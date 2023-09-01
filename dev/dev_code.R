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

# count_int() var_integer()? ---------
summarise_if(dt2, is.numeric, count_int) %>% pivot_longer(cols = everything())

# datetime_summary() -------
#Explore which are simple datetimes. Maybe also check which are 0 in Excel i.e.
# equal as.POSIXct("1899-12-31", tz = "UTC")
summarise_if(dt, is.POSIXct, is_simple_datetime) %>% pivot_longer(cols = everything())

# here()----------
# See loadable() https://github.com/yihui/xfun/blob/main/R/packages.R

#dir_contents--------
library(tidyverse)
library(fs)

fs::dir_info(".", recurse = 1) %>% select(path)
fs::dir_info(getwd(), recurse = 0) %>% select(path)
dir_files("/home/james/rrr/edwards2")


dir_files(c(".", "/home/james/rrr/edwards/R"))
fs::dir_info(c("/home/james/rrr/edwards/R"))

dir_files(".", recurse = 1) %>% prinf()
dir_files(getwd(), recurse = 1)

dir_files(".", recurse = 1)
dir_files(".", recurse = 1)
dir_files(getwd(), recurse = 1)
dir_files(".", recurse = 1, shorten = F)
dir_files(getwd(), recurse = 1, shorten = F)

x <- factor(3:5)
as.numeric(x)
factor_to_numeric(x)
factor_to_numeric(factor(c("T", "false", NA)))
factor_to_numeric(factor(c("T", "1")))
