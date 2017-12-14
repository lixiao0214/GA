
parents<-list()
parents[[1]]<-rbind(rbinom(n = 9, size = 1, prob = 0.5),
                    rbinom(n = 9, size = 1, prob = 0.5))

test_that("test crossover_p_splt works",
          expect_true(
            is.matrix(crossover_p_split(parents,p=2))
            ))

