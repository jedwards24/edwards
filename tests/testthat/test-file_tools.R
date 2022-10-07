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
