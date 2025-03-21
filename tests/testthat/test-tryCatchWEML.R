test_that("tryCatchWEML works as expected", {
  expect_equal(
    tryCatchWEML(2+2),
    list(value    = 4,
         messages = character(0),
         warnings = character(0),
         errors   = character(0),
         log      = character(0))
  )
})
