tb <- tibble::tibble(group = c(rep("a", 25), rep("b", 15), rep("c", 10)),
             outcome = c(rep(0L, 20), rep(1L, 15), rep(0L, 10), rep(1L, 5))) %>%
  dplyr::mutate(groupf = factor(group)) %>%
  dplyr::mutate(outf = factor(outcome)) %>%
  dplyr::mutate(outb = as.logical(outcome)) %>%
  dplyr::mutate(outbf = factor(outb)) %>%
  dplyr::mutate(outc = ifelse(outb, "yes", "no"))
n_vals <- length(unique(tb$group))
ref <- suppressWarnings(suppressMessages(prop_ci(tb, "outcome", "group", plot = FALSE)))

test_that("prop_ci works", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_s3_class(ref, "data.frame")
  expect_s3_class(suppressMessages(prop_ci(tb, "outcome", "group", return_plot = TRUE)), "ggplot")
  expect_identical(nrow(ref), n_vals)
  expect_true(all(ref$prop >= 0 & ref$prop <= 1))
  expect_true(all(ref$lo <= ref$prop))
  expect_true(all(ref$hi >= ref$prop))
})

test_that("prop_ci handles inputs", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_identical(suppressMessages(prop_ci(tb, "outf", "group", plot = FALSE)), ref)
  expect_identical(suppressMessages(prop_ci(tb, "outb", "group", plot = FALSE)), ref)
  expect_identical(suppressMessages(prop_ci(tb, "outbf", "group", pos_class = "TRUE", plot = FALSE)), ref)
  expect_identical(suppressMessages(prop_ci(tb, "outc", "group", pos_class = "yes", plot = FALSE)), ref)
  expect_false(identical(suppressMessages(prop_ci(tb, "outc", "group", plot = FALSE)), ref))
  expect_message(prop_ci(tb, "outc", "group", plot = FALSE), "Target not binary or logical. Treating")
  # next tests predictor var as factor. Not identical since "value" in returned df is factor
  #expect_identical(prop_ci(tb, "outcome", "groupf"), ref)
  tb2 <- tibble::add_row(tb, group = "c", outcome = 2)
  expect_error(suppressMessages(prop_ci(tb2, "outcome", "group", plot = FALSE)), "Target variable must be binary.")
})
