

test_that("fgh is not a currency_code", {
  expect_equal(get_exchange_rates("fgh"), NA)
})