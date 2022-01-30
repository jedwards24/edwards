
# plot missing--------
var_summary(attenu)
plot_missing2(attenu)
library(data.table)

# find_similar ----------
# See functions in `dev_find_similar()`
library(dplyr)
dt <- tibble::tibble(a = 1:5, b = 1:5, c = c(1, 2, 5, 4, 3), d = 5:1) %>%
  mutate_all(as.integer)
dt2 <- tibble::tibble(a = 1:5, b = c(NA, 2:5), c = c(0, 1, 1, 2, NA), d = c(0, 0, 1, 2, NA)) %>%
  mutate_all(as.integer)
dt3 <- dt %>% mutate(e = letters[1:5])
x1 <- find_similar_single(dt)
x2 <- find_similar_single(dt2)
x3 <- find_similar_single(dt3)

x4 <- find_similar_single4(dt)
x5 <- find_similar_single4(dt2)
x6 <- find_similar_single4(dt3)

x7 <- find_similar_single3(dt)
x8 <- find_similar_single3(dt2)
x9 <- find_similar_single3(dt3)

identical(x1, x4)
identical(x2, x5)
identical(x3, x6)
identical(x1, x7)
identical(x2, x8)
identical(x3, x9)

waldo::compare(x1, x4)

nn <- 1000
dt4 <- tibble::tibble(a = sample.int(3, size = nn, replace = TRUE),
                      b = sample.int(3, size = nn, replace = TRUE),
                      u = sample(letters[1:3], size = nn, replace = TRUE),
                      v = sample(letters[1:3], size = nn, replace = TRUE),
                      w = sample(c(NA, letters[1:2]), size = nn, replace = TRUE))
r1 <- find_similar_single(dt4)
r2 <- find_similar_single2(dt4)
r3 <- find_similar_single3(dt4)
r4 <- find_similar_single4(dt4)

identical(r1, r2)
identical(r1, r3)
identical(r1, r4)

dt5 <- matrix(rpois(3000, 1), ncol = 30) %>%
  as_tibble()

# small examples
library(microbenchmark)
mb <- microbenchmark(f1 = find_similar_single(dt4),
                     f2 = find_similar_single2(dt4),
                     f3 = find_similar_single3(dt4),
                     f4 = find_similar_single4(dt4))
mb
plot(mb)
# This is a better bm
mb <- microbenchmark(f1 = find_similar_single(dt5),
                     f2 = find_similar_single2(dt5),
                     f3 = find_similar_single3(dt5),
                     f4 = find_similar_single4(dt5),
                     times = 5)
mb

library(profvis)
profvis(find_similar_single4(dt5))

similar_col_single(dt2$c, dt2$d, "d")
similar_col_single3(dt2$c, dt2$d)

find_similar_single2(dt2)
x2
identical(x2, x4)

purrr::map_chr(dt, class)
select(dt3, where(~class(.) == "character"))
select(dt3, where(is.character))
purrr::map(dt3, ~class(.) == "character")
dt %>%
  purrr::map(pull) %>%
  purrr::map(names)

list(1:5, 2:6, 3:7) %>% do.call(rbind, .)
list(1:5) %>% do.call(rbind, .)

purrr::map_int(dt, ~sum(. == dt$a, na.rm = TRUE))
library(purrr)
?map
mutate_all(dt, unname)
