source("./clock.R")
library(testthat)

context("clock")

# Create a new clock with an initial time

test_that("on the hour", {
  clock <- create(8, 0)
  expect_equal(display(clock), "08:00")
})

test_that("past the hour", {
  clock <- create(11, 9)
  expect_equal(display(clock), "11:09")
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

test_that("negative minutes", {
  clock <- create(1, -40)
  expect_equal(display(clock), "00:20")
})

test_that("negative minutes roll over", {
  clock <- create(1, -160)
  expect_equal(display(clock), "22:20")
})

test_that("negative minutes roll over continuously", {
  clock <- create(1, -4820)
  expect_equal(display(clock), "16:40")
})

test_that("negative sixty minutes is previous hour", {
  clock <- create(2, -60)
  expect_equal(display(clock), "01:00")
})

test_that("negative hour and minutes both roll over", {
  clock <- create(-25, -160)
  expect_equal(display(clock), "20:20")
})

test_that("negative hour and minutes both roll over continuously", {
  clock <- create(-121, -5810)
  expect_equal(display(clock), "22:10")
})

# Add minutes
test_that("add minutes", {
  clock <- create(10, 0)
  expect_equal(display(add(clock, 3)), "10:03")
})

test_that("add no minutes", {
  clock <- create(6, 41)
  expect_equal(display(add(clock, 0)), "06:41")
})

test_that("add to next hour", {
  clock <- create(0, 45)
  expect_equal(display(add(clock, 40)), "01:25")
})

test_that("add more than one hour", {
  clock <- create(10, 0)
  expect_equal(display(add(clock, 61)), "11:01")
})

test_that("add more than two hours with carry", {
  clock <- create(0, 45)
  expect_equal(display(add(clock, 160)), "03:25")
})

test_that("add across midnight", {
  clock <- create(23, 59)
  expect_equal(display(add(clock, 2)), "00:01")
})

test_that("add more than one day (1500 min = 25 hrs)", {
  clock <- create(5, 32)
  expect_equal(display(add(clock, 1500)), "06:32")
})

test_that("add more than two days", {
  clock <- create(1, 1)
  expect_equal(display(add(clock, 3500)), "11:21")
})


# Subtract minutes
test_that("subtract minutes", {
  clock <- create(10, 3)
  expect_equal(display(subtract(clock, 3)), "10:00")
})

test_that("subtract to previous hour", {
  clock <- create(10, 3)
  expect_equal(display(subtract(clock, 30)), "09:33")
})

test_that("subtract more than an hour", {
  clock <- create(10, 3)
  expect_equal(display(subtract(clock, 70)), "08:53")
})

test_that("subtract across midnight", {
  clock <- create(0, 3)
  expect_equal(display(subtract(clock, 4)), "23:59")
})

test_that("subtract more than two hours", {
  clock <- create(0, 0)
  expect_equal(display(subtract(clock, 160)), "21:20")
})

test_that("subtract more than two hours with borrow", {
  clock <- create(6, 15)
  expect_equal(display(subtract(clock, 160)), "03:35")
})

test_that("subtract more than one day (1500 min = 25 hrs)", {
  clock <- create(5, 32)
  expect_equal(display(subtract(clock, 1500)), "04:32")
})

test_that("subtract more than two days", {
  clock <- create(2, 20)
  expect_equal(display(subtract(clock, 3000)), "00:20")
})

# Compare two clocks for equality

test_that("clocks with same time", {
  clock1 <- create(15, 37)
  clock2 <- create(15, 37)
  expect_equal(equal(clock1, clock2), TRUE)
})

test_that("clocks a minute apart", {
  clock1 <- create(15, 36)
  clock2 <- create(15, 37)
  expect_equal(equal(clock1, clock2), FALSE)
})

test_that("clocks an hour apart", {
  clock1 <- create(14, 37)
  clock2 <- create(15, 37)
  expect_equal(equal(clock1, clock2), FALSE)
})

test_that("clocks with hour overflow", {
  clock1 <- create(10, 37)
  clock2 <- create(34, 37)
  expect_equal(equal(clock1, clock2), TRUE)
})

test_that("clocks with hour overflow by several days", {
  clock1 <- create(3, 11)
  clock2 <- create(99, 11)
  expect_equal(equal(clock1, clock2), TRUE)
})

test_that("clocks with negative hour", {
  clock1 <- create(22, 40)
  clock2 <- create(-2, 40)
  expect_equal(equal(clock1, clock2), TRUE)
})

test_that("clocks with negative hour that wraps", {
  clock1 <- create(17, 3)
  clock2 <- create(-31, 3)
  expect_equal(equal(clock1, clock2), TRUE)
})

test_that("clocks with negative hour that wraps multiple times", {
  clock1 <- create(13, 49)
  clock2 <- create(-83, 49)
  expect_equal(equal(clock1, clock2), TRUE)
})

test_that("clocks with minute overflow", {
  clock1 <- create(0, 1)
  clock2 <- create(0, 1441)
  expect_equal(equal(clock1, clock2), TRUE)
})

test_that("clocks with minute overflow by several days", {
  clock1 <- create(2, 2)
  clock2 <- create(2, 4322)
  expect_equal(equal(clock1, clock2), TRUE)
})

test_that("clocks with negative minute", {
  clock1 <- create(2, 40)
  clock2 <- create(3, -20)
  expect_equal(equal(clock1, clock2), TRUE)
})

test_that("clocks with negative minute that wraps", {
  clock1 <- create(4, 10)
  clock2 <- create(5, -1490)
  expect_equal(equal(clock1, clock2), TRUE)
})

test_that("clocks with negative minute that wraps multiple times", {
  clock1 <- create(6, 15)
  clock2 <- create(6, -4305)
  expect_equal(equal(clock1, clock2), TRUE)
})

test_that("clocks with negative hours and minutes", {
  clock1 <- create(7, 32)
  clock2 <- create(-12, -268)
  expect_equal(equal(clock1, clock2), TRUE)
})

test_that("clocks with negative hours and minutes that wrap", {
  clock1 <- create(18, 7)
  clock2 <- create(-54, -11513)
  expect_equal(equal(clock1, clock2), TRUE)
})

test_that("full clock and zeroed clock", {
  clock1 <- create(24, 0)
  clock2 <- create(0, 0)
  expect_equal(equal(clock1, clock2), TRUE)
})

message("All tests passed for exercise: clock")
