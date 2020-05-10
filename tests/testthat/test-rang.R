context("rang_ functions")

set.seed(21)
dt <- ggplot2::diamonds %>%
  dplyr::mutate(top = ifelse(cut == "Ideal", 1, 0) %>% factor(levels = c(1, 0))) %>%
  select(-cut) %>%
  sample_n(100)

test_that("rang_oob_error() works", {
  rf <- ranger::ranger(top ~ . , dt, seed = 20, keep.inbag = T, num.trees = 200)
  expect_known_hash(rang_oob_err(rf, dt, plot = FALSE), hash = "5cb256f2ca")
})

test_that("rang_roc_cut() works", {
  rf <- ranger::ranger(top ~ . , dt, seed = 20, num.trees = 100, probability = T)
  expect_equal(rang_roc_cut(rf, dt$top, plot = FALSE),
         c(sensitivity = 0.8837209, specificity = 0.8947368, cutoff = 0.4741270, auc = 0.8816809),
         tolerance = 1e-6)
})


