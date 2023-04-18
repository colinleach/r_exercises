source("./clock.R")
library(testthat)

context("clock")

# Create a new clock with an initial time

test_that("on the hour", {
  clock <- create(8, 0)
  expect_equal(display(clock), "08:00")
})

test_that("midnight is zero hours", {
  clock <- create(24, 0)
  expect_equal(display(clock), "00:00")
})

test_that("hour rolls over", {
  clock <- create(25, 0)
  expect_equal(display(clock), "01:00")
})

test_that("hour rolls over continuously", {
  clock <- create(100, 0)
  expect_equal(display(clock), "04:00")
})

test_that("sixty minutes is next hour", {
  clock <- create(1, 60)
  expect_equal(display(clock), "02:00")
})

test_that("minutes roll over", {
  clock <- create(0, 160)
  expect_equal(display(clock), "02:40")
})

test_that("minutes roll over continuously", {
  clock <- create(0, 1723)
  expect_equal(display(clock), "04:43")
})

test_that("hour and minutes roll over", {
  clock <- create(25, 160)
  expect_equal(display(clock), "03:40")
})

test_that("hour and minutes roll over continuously", {
  clock <- create(201, 3001)
  expect_equal(display(clock), "11:01")
})

test_that("hour and minutes roll over to exactly midnight", {
  clock <- create(72, 8640)
  expect_equal(display(clock), "00:00")
})

test_that("negative hour", {
  clock <- create(-1, 15)
  expect_equal(display(clock), "23:15")
})

test_that("negative hour rolls over", {
  clock <- create(-25, 0)
  expect_equal(display(clock), "23:00")
})

test_that("negative hour rolls over continuously", {
  clock <- create(-91, 0)
  expect_equal(display(clock), "05:00")
})

# test_that("", {
#   clock <- create(201, 3001)
#   expect_equal(display(clock), "11:01")
# })
#
# test_that("", {
#   clock <- create(201, 3001)
#   expect_equal(display(clock), "11:01")
# })
#
# test_that("", {
#   clock <- create(201, 3001)
#   expect_equal(display(clock), "11:01")
# })
#
# test_that("", {
#   clock <- create(201, 3001)
#   expect_equal(display(clock), "11:01")
# })
#
# test_that("", {
#   clock <- create(201, 3001)
#   expect_equal(display(clock), "11:01")
# })
#
# test_that("", {
#   clock <- create(201, 3001)
#   expect_equal(display(clock), "11:01")
# })
#
# test_that("", {
#   clock <- create(201, 3001)
#   expect_equal(display(clock), "11:01")
# })
#
# test_that("", {
#   clock <- create(201, 3001)
#   expect_equal(display(clock), "11:01")
# })
#
# test_that("", {
#   clock <- create(201, 3001)
#   expect_equal(display(clock), "11:01")
# })
#
# test_that("", {
#   clock <- create(201, 3001)
#   expect_equal(display(clock), "11:01")
# })
#
# test_that("", {
#   clock <- create(201, 3001)
#   expect_equal(display(clock), "11:01")
# })
#
# test_that("", {
#   clock <- create(201, 3001)
#   expect_equal(display(clock), "11:01")
# })
#
# test_that("", {
#   clock <- create(201, 3001)
#   expect_equal(display(clock), "11:01")
# })
#
# test_that("", {
#   clock <- create(201, 3001)
#   expect_equal(display(clock), "11:01")
# })
#
# test_that("", {
#   clock <- create(201, 3001)
#   expect_equal(display(clock), "11:01")
# })
#
# test_that("", {
#   clock <- create(201, 3001)
#   expect_equal(display(clock), "11:01")
# })
#
# test_that("", {
#   clock <- create(201, 3001)
#   expect_equal(display(clock), "11:01")
# })
#


message("All tests passed for exercise: clock")
