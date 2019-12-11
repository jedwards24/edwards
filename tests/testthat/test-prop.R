context("Prop functions")

library(tidyverse)
tb <- tibble(group = c(rep("a", 25), rep("b", 15), rep("c", 10)),
             outcome = c(rep(0, 20), rep(1, 15), rep(0, 10), rep(1, 5))) %>%
  mutate(groupf = factor(group)) %>%
  mutate(outf = factor(outcome)) %>%
  mutate(outb = as.logical(outcome)) %>%
  mutate(outbf = factor(outb)) %>%
  mutate(outc = ifelse(outb, "yes", "no"))
n_vals <- length(unique(tb$group))
ref <- prop_ci(tb, "outcome", "group")

test_that("prop_ci works", {
  expect_s3_class(ref, "data.frame")
  expect_identical(nrow(ref), n_vals)
  expect_true(all(ref$prop >= 0 & ref$prop <= 1))
  expect_true(all(ref$lo <= ref$prop))
  expect_true(all(ref$hi >= ref$prop))
})

test_that("prop_ci handles inputs", {
  expect_identical(prop_ci(tb, "outf", "group"), ref)
  expect_identical(prop_ci(tb, "outb", "group"), ref)
  expect_identical(prop_ci(tb, "outbf", "group", pos_class = "TRUE"), ref)
  expect_identical(prop_ci(tb, "outc", "group", pos_class = "yes"), ref)
  expect_false(identical(prop_ci(tb, "outc", "group"), ref))
  expect_message(prop_ci(tb, "outc", "group"), "Target not binary or logical. Treating")
  # next tests predictor var as factor. Not identical since "value" in returned df is factor
  #expect_identical(prop_ci(tb, "outcome", "groupf"), ref)
  tb2 <- add_row(tb, group = "c", outcome = 2)
  expect_error(prop_ci(tb2, "outcome", "group"), "Target variable must be binary.")
})