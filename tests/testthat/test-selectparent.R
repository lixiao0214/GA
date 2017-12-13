library(testthat)
testparents <- matrix(rep(c(1,0,0,1),5), nrow = 5)
testcouplenum <- 10
testfitness <- c(1,2,3,4,5)
testsubsetnum <- 4

test_that("test twopropselection works",
          expect_true(
            is.list(selectparents(
              originalparents=testparents,
              couplenum = testcouplenum,
              method = "twopropselection",
              fitness = testfitness))))

test_that("test onepropselection works",
          expect_true(
            is.list(selectparents(
              originalparents=testparents,
              couplenum = testcouplenum,
              method = "onepropselection",
              fitness = testfitness))))

test_that("test tournament works",
          expect_true(
            is.list(selectparents(
              originalparents=testparents,
              couplenum = testcouplenum,
              method = "tournament",
              fitness = testfitness,
              subsetnum = testsubsetnum
            ))))

test_that("test tournament fails without subsetnum",
          expect_error(selectparents(originalparents=testparents,
                                     couplenum = testcouplenum,
                                     method = "tournament",
                                     fitness = testfitness)))
