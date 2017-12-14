plot_final <- function(input){
  #' Plot goodness of fit vs generation
  #'
  #' This function works to plot goodness of fit against generation
  #'
  #' @author Xiao Li
  #'
  #' @param input It is a list. The first element of the list contains all values of goodness of fit
  #' during different generations.
  #'
  require(ggplot2)
  m <- matrix(unlist(input[[1]]),nrow = length(input[[1]]),byrow = T)
  pts <- data.frame()
  for(i in 1:nrow(m)){
    pts <- rbind(pts,data.frame(x=rep(i,length(m[i,])),y=m[i,]))
  }

  g <- ggplot(data = pts, aes(x=x,y=y)) +
    geom_point() +
    xlab("Generations") +
    ylab("Goodness of Fit") +
    stat_summary(aes(y = y,group=1), fun.y=mean, colour="red", geom="line",group=1)
  return(g)
}

# Fixing all parameters except mutation rate
mutation_001 <- main(data=mtcars,
                     method = "onepropselection",
                     p=2,
                     mutation_rate = 0.01,
                     regression_target = "mpg",
                     scheme = "re-rank",
                     max_iter = 500)

mutation_010 <- main(data=mtcars,
                     method = "onepropselection",
                     p=2,
                     mutation_rate = 0.1,
                     regression_target = "mpg",
                     scheme = "re-rank",
                     max_iter = 500)

plot_final(mutation_001)
plot_final(mutation_010)

# Fixing all parameters except number of partitions in crossover
p_2 <- main(data=mtcars,
            method = "onepropselection",
            p=2,
            mutation_rate = 0.01,
            regression_target = "mpg",
            scheme = "re-rank",
            max_iter = 500)

p_5 <- main(data=mtcars,
            method = "onepropselection",
            p=5,
            mutation_rate = 0.01,
            regression_target = "mpg",
            scheme = "re-rank",
            max_iter = 500)

plot_final(p_2)
plot_final(p_5)

# Fixing all parameters except method of selecting parents
select_1 <- main(data=mtcars,
                 method = "onepropselection",
                 p=2,
                 mutation_rate = 0.01,
                 regression_target = "mpg",
                 scheme = "re-rank",
                 max_iter = 500)

select_2 <- main(data=mtcars,
                 method = "twopropselection",
                 p=2,
                 mutation_rate = 0.01,
                 regression_target = "mpg",
                 scheme = "re-rank",
                 max_iter = 500)

select_3 <- main(data=mtcars,
                 method = "tournament",
                 p=2,
                 mutation_rate = 0.01,
                 regression_target = "mpg",
                 scheme = "re-rank",
                 max_iter = 500)

plot_final(select_1)
plot_final(select_2)
plot_final(select_3)

# Fixing all parameters except method of replacing population
replace_prop <- main(data=mtcars,
                     method = "tournament",
                     p=2,
                     mutation_rate = 0.01,
                     regression_target = "mpg",
                     scheme = "proportion",
                     max_iter = 500)

replace_rerank <- main(data=mtcars,
                       method = "tournament",
                       p=2,
                       mutation_rate = 0.01,
                       regression_target = "mpg",
                       scheme = "re-rank",
                       max_iter = 500)

plot_final(replace_prop)
plot_final(replace_rerank)
