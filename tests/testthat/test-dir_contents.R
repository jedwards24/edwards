test_that("dir_size() runs", {
  expect_type(dir_size("../testdata"), "double")
})

test_that("dir_contents() is correct", {
  x1 <- dir_contents(".", recurse = 1)
  x2 <- dir_contents(getwd(), recurse = 1)
  x3 <- dir_contents(paste0(getwd(), "/"), recurse = 1)
  expect_true(all(x1$level %in% 0:1))
  expect_equal(x1$level[1], 0L)
  expect_type(x1, "list")
  expect_identical(x1, x2, ignore_attr = "call")
  expect_identical(x1, x3, ignore_attr = "call")
  x4 <- dir_contents(".", recurse = 1, shorten = FALSE)
  x5 <- dir_contents(getwd(), recurse = 1, shorten = FALSE)
  x6 <- dir_contents(paste0(getwd(), "/"), recurse = 1, shorten = FALSE)
  expect_identical(x1[, -1], x4[, -1], ignore_attr = "call")
  expect_identical(x4, x5, ignore_attr = "call")
  expect_identical(x4, x6, ignore_attr = "call")
})

test_that("dir_files() is correct", {
  x1 <- dir_files(".", recurse = 1)
  x2 <- dir_files(getwd(), recurse = 1)
  x3 <- dir_files(paste0(getwd(), "/"), recurse = 1)
  expect_true(all(x1$level %in% 0:1))
  expect_type(x1, "list")
  expect_identical(x1, x2, ignore_attr = "call")
  expect_identical(x1, x3, ignore_attr = "call")
  x4 <- dir_files(".", recurse = 1, shorten = FALSE)
  x5 <- dir_files(getwd(), recurse = 1, shorten = FALSE)
  x6 <- dir_files(paste0(getwd(), "/"), recurse = 1, shorten = FALSE)
  expect_identical(subset(x1, select = -directory), subset(x4, select = -directory), ignore_attr = "call")
  expect_identical(x4, x5, ignore_attr = "call")
  expect_identical(x4, x6, ignore_attr = "call")
})
