
context("Testing trendex")

set.seed(1)

test_that("bogus arguments throw error",{
  expect_error(trendex("abc"))
  expect_error(trendex(1:3, threads = 1))
  expect_error(trendex(1:3, num_siftings = 0, S_number = 0, threads = 1))
  expect_error(trendex(1:3, num_imfs = -1, threads = 1))
  expect_error(trendex(1:3, num_siftings = -3, threads = 1))
  expect_error(trendex(1:3, ensemble_size = 0, threads = 1))
  expect_error(trendex(1:3, num_imfs = "lots", threads = 1))
})

