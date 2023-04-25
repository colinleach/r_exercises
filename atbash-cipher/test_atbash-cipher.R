source("./atbash-cipher.R")
library(testthat)

context("atbash-cipher")

# The tests are divided into two groups:
# * Encoding from English to atbash cipher",
# * Decoding from atbash cipher to all-lowercase-mashed-together English

# Encode

test_that("encode yes", {
  input <- "yes"
  expected <- "bvh"
  expect_equal(encode(input), expected)
})

test_that("encode no", {
  input <- "no"
  expected <- "ml"
  expect_equal(encode(input), expected)
})

test_that("encode OMG", {
  input <- "OMG"
  expected <- "lnt"
  expect_equal(encode(input), expected)
})

test_that("encode spaces", {
  input <- "O M G"
  expected <- "lnt"
  expect_equal(encode(input), expected)
})

test_that("encode mindblowingly", {
  input <- "mindblowingly"
  expected <- "nrmwy oldrm tob"
  expect_equal(encode(input), expected)
})

test_that("encode numbers", {
  input <- "Testing,1 2 3, testing."
  expected <- "gvhgr mt123 gvhgr mt"
  expect_equal(encode(input), expected)
})

test_that("encode deep thought", {
  input <- "uth is fiction."
  expected <- "gifgs rhurx grlm"
  expect_equal(encode(input), expected)
})

test_that("encode all the letters", {
  input <- "The quick brown fox jumps over the lazy dog."
  expected <- "gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt"
  expect_equal(encode(input), expected)
})

# Decode

test_that("decode exercism", {
  input <- "vcvix rhn"
  expected <- "exercism"
  expect_equal(decode(input), expected)
})

test_that("decode a sentence", {
  input <- "zmlyh gzxov rhlug vmzhg vkkrm thglm v"
  expected <- "anobstacleisoftenasteppingstone"
  expect_equal(decode(input), expected)
})

test_that("decode numbers", {
  input <- "gvhgr mt123 gvhgr mt"
  expected <- "testing123testing"
  expect_equal(decode(input), expected)
})

test_that("decode all the letters", {
  input <- "gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt"
  expected <- "thequickbrownfoxjumpsoverthelazydog"
  expect_equal(decode(input), expected)
})

test_that("decode with too many spaces", {
  input <- "vc vix    r hn"
  expected <- "exercism"
  expect_equal(decode(input), expected)
})

test_that("decode with no spaces", {
  input <- "zmlyhgzxovrhlugvmzhgvkkrmthglmv"
  expected <- "anobstacleisoftenasteppingstone"
  expect_equal(decode(input), expected)
})

message("All tests passed for exercise: atbash-cipher")
