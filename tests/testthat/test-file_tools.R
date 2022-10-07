test_that("latest_file() works", {
  dir <- "../testdata/file_tools"
  out <- fs::path(dir,
           c("testmore_01.RDS",
             "test_221.RDS",
             "test_22.RDS",
             "sub/test_88.RDS",
             "sub/test_00.RDS",
             "test_11.RDATA"))
  names(out) <- out
  expect_equal(latest_file(dir), out[1])
  expect_equal(latest_file(dir, "test"), out[1])
  expect_equal(latest_file(dir, "test_"), out[2])
  expect_equal(latest_file(dir, "test_\\d{2}\\D"), out[3])
  expect_equal(latest_file(dir, "test_", recurse = TRUE), out[4])
  expect_equal(latest_file(dir, "test_", recurse = TRUE, decreasing = FALSE), out[5])
  expect_equal(latest_file(dir, "test_", ext = "rdata"), out[6])
  expect_equal(latest_file(dir, "test_", n = 1:2), out[2:3])

  expect_error(latest_file(dir, "whereisit"), "fewer than `n`.*No files match")
  expect_error(latest_file(dir, "test_\\d{3}", n = 2), "fewer than `n`.*test_221")
})

test_that("save_check() does not overwrite incorrectly", {
  testthat::expect_message(save_check(1, "../testdata/file_tools/test_20.RDS", version = 2),
                           "Output not saved")
})

test_that("dir_...() functions run", {
  expect_type(dir_files("../testdata"), "list")
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


