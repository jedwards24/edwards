dir.create("tests/testdata/file_tools")
x <- 1L
save(x, file = "tests/testdata/file_tools/test_11.RDATA")
?save
saveRDS(1L, "tests/testdata/file_tools/test_22.RDS", version = 2)
saveRDS(1L, "tests/testdata/file_tools/test_221.RDS", version = 2)
saveRDS(1L, "tests/testdata/file_tools/testmore_01.RDS", version = 2)
dir.create("tests/testdata/file_tools/sub")
saveRDS(1L, "tests/testdata/file_tools/sub/test_00.RDS", version = 2)
saveRDS(1L, "tests/testdata/file_tools/sub/test_88.RDS", version = 2)

