source("./isbn-verifier.R")
library(testthat)

context("isbn-verifier")

# An expected value of true indicates a valid ISBN-10,
# whereas false means the ISBN-10 is invalid.

test_that("valid isbn", {
  isbn <- "3-598-21508-8"
  result <- TRUE
  expect_equal(is_valid(isbn), result)
})

test_that("invalid isbn check digit", {
  isbn <- "3-598-21508-9"
  result <- FALSE
  expect_equal(is_valid(isbn), result)
})

test_that("valid isbn with a check digit of 10", {
  isbn <- "3-598-21507-X"
  result <- TRUE
  expect_equal(is_valid(isbn), result)
})

test_that("check digit is a character other than X", {
  isbn <- "3-598-21507-A"
  result <- FALSE
  expect_equal(is_valid(isbn), result)
})

test_that("invalid check digit in isbn is not treated as zero", {
  isbn <- "4-598-21507-B"
  result <- FALSE
  expect_equal(is_valid(isbn), result)
})

test_that("invalid character in isbn is not treated as zero", {
  isbn <- "3-598-P1581-X"
  result <- FALSE
  expect_equal(is_valid(isbn), result)
})

test_that("X is only valid as a check digit", {
  isbn <- "3-598-2X507-9"
  result <- FALSE
  expect_equal(is_valid(isbn), result)
})

test_that("valid isbn without separating dashes", {
  isbn <- "3598215088"
  result <- TRUE
  expect_equal(is_valid(isbn), result)
})

test_that("isbn without separating dashes and X as check digit", {
  isbn <- "359821507X"
  result <- TRUE
  expect_equal(is_valid(isbn), result)
})

test_that("isbn without check digit and dashes", {
  isbn <- "359821507"
  result <- FALSE
  expect_equal(is_valid(isbn), result)
})

test_that("too long isbn and no dashes", {
  isbn <- "3598215078X"
  result <- FALSE
  expect_equal(is_valid(isbn), result)
})

test_that("too short isbn", {
  isbn <- "00"
  result <- FALSE
  expect_equal(is_valid(isbn), result)
})

test_that("isbn without check digit", {
  isbn <- "3-598-21507"
  result <- FALSE
  expect_equal(is_valid(isbn), result)
})

test_that("check digit of X should not be used for 0", {
  isbn <- "3-598-21515-X"
  result <- FALSE
  expect_equal(is_valid(isbn), result)
})

test_that("empty isbn", {
  isbn <- ""
  result <- FALSE
  expect_equal(is_valid(isbn), result)
})

test_that("input is 9 characters", {
  isbn <- "134456729"
  result <- FALSE
  expect_equal(is_valid(isbn), result)
})

test_that("invalid characters are not ignored after checking length", {
  isbn <- "3132P34035"
  result <- FALSE
  expect_equal(is_valid(isbn), result)
})

test_that("invalid characters are not ignored before checking length", {
  isbn <- "3598P215088"
  result <- FALSE
  expect_equal(is_valid(isbn), result)
})

test_that("input is too long but contains a valid isbn", {
  isbn <- "98245726788"
  result <- FALSE
  expect_equal(is_valid(isbn), result)
})

message("All tests passed for exercise: isbn-verifier")
