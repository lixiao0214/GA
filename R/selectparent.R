twopropselection <- function(originalparents, couplenum, fitness){
  #' twopropselection
  #'
  #' Select one parent with probability proportional to fitness(or fitnessrank)
  #'and to select the other parent completely at random
  #' @return Returns the selected parents ~ list
  #' @author Lei Zhang
  #' @param originalparents a casenum * variablenum matrix
  #' @param couplenum number of couples selected
  #' @param fitness goodness of fit ~ a casenum * 1 vector , default is AICvalue
  #' @example twopropselection(originalparents = matrix(rep(c(1,0,0,1),5), nrow = 5), couplenum = 5, fitness = c(1,2,3,4,5))

  stopifnot(is.matrix(originalparents))
  stopifnot(is.numeric(couplenum))
  stopifnot(is.vector(fitness))


  firstparent <- originalparents[sample(1:nrow(originalparents), size = couplenum, replace = TRUE,                                       prob = fitness),]
  secondparent <- originalparents[sample(1:nrow(originalparents), size = couplenum, replace = TRUE),]
  #transform the each row of the matrix into a list
  firstparent <- split(firstparent, row(firstparent))
  secondparent <- split(secondparent, row(secondparent))
  #combine firstparent and secondparent to form the list of paired parents
  newcouple <- mapply(rbind, firstparent, secondparent, SIMPLIFY=FALSE)
  return(newcouple)
}



onepropselection <- function(originalparents, couplenum, fitness){
  #' onepropselection
  #'
  #' Select one parent with probability proportional to fitness and to select
  #' the other parent completely at random
  #'
  #' @return Returns the selected parents ~ list
  #' @author Lei Zhang
  #' @param originalparents a casenum * variablenum matrix
  #' @param couplenum number of couples selected
  #' @param fitness goodness of fit ~ a casenum * 1 vector , default is AICvalue
  #' @example onepropselection(originalparents = matrix(rep(c(1,0,0,1),5), nrow = 5), couplenum = 5, fitness = c(1,2,3,4,5))

  stopifnot(is.matrix(originalparents))
  stopifnot(is.numeric(couplenum))
  stopifnot(is.vector(fitness))

  firstparent <- originalparents[sample(1:nrow(originalparents), size = couplenum, replace = TRUE, prob = fitness),]
  secondparent <- originalparents[sample(1:nrow(originalparents), size = couplenum, replace = TRUE, prob = fitness),]
  #transform the each row of the matrix into a list
  firstparent <- split(firstparent, row(firstparent))
  secondparent <- split(secondparent, row(secondparent))
  #combine firstparent and secondparent to form the list of paired parents
  newcouple <- mapply(rbind, firstparent, secondparent, SIMPLIFY=FALSE)
  return(newcouple)
}

tournament <- function(originalparents, couplenum, subsetnum, fitness){
  #' tournament selection
  #'
  #' the set of parents in generation t is randomly partitioned into k disjoint
  #' subsets of equal size(perhaps with a few remaining parents temporarily ignored)
  #' The best individual in each group is chosen as a parent.
  #' Additional random partitionings are carried out until sufficient parents have
  #' been generated.Parents are then paired randomly for breeding.
  #'
  #' @return Returns the selected parents ~ list
  #' @author Lei Zhang
  #' @param originalparents a casenum * variablenum matrix
  #' @param couplenum number of couples selected
  #' @param subsetnum partition the set of chromosomes in generation t into "subsetnum" disjoint subsets
  #' @param fitness goodness of fit ~ a casenum * 1 vector , default is AICvalue
  #' @example tournament(originalparents = matrix(rep(c(1,0,0,1),5), nrow = 5), couplenum = 5, fitness = c(1,2,3,4,5), subsetnum = 2)

  stopifnot(is.matrix(originalparents))
  stopifnot(is.numeric(couplenum))
  stopifnot(is.vector(fitness))
  stopifnot(is.numeric(subsetnum))

  partitiontime <- ceiling(couplenum*2/subsetnum)
  parentspool <- matrix(0, partitiontime*subsetnum, ncol(originalparents))
  getsubpars <- function(originalparents,subsetnum){
    totalrownum <- nrow(originalparents)
    index <- sample(1:nrow(originalparents), (floor(totalrownum/subsetnum)*subsetnum), replace = FALSE)
    indexmatrix <- matrix(index, ncol = subsetnum)
    fitnessmatrix <- matrix(fitness[index], ncol = subsetnum)
    colmaxindex <- apply(fitnessmatrix, 2, which.max )
    selectedindex <- rep(0, ncol(indexmatrix))
    for (i in 1:ncol(indexmatrix)){
      selectedindex[i] <- indexmatrix[colmaxindex[i],i]
    }
    return(originalparents[selectedindex,])
  }
  for (i in 1:partitiontime){
    parentspool[((i-1)*subsetnum+1):(i*subsetnum),] <- getsubpars(originalparents = originalparents, subsetnum = subsetnum)
  }
  parentspool <- parentspool[1:(couplenum*2),]
  firstparentindex <- sample(1:(couplenum*2), size = couplenum, replace = FALSE)
  firstparent <- parentspool[firstparentindex,]
  secondparent <- parentspool[-firstparentindex,]
  #transform the each row of the matrix into a list
  firstparent <- split(firstparent, row(firstparent))
  secondparent <- split(secondparent, row(secondparent))
  #combine firstparent and secondparent to form the list of paired parents
  newcouple <- mapply(rbind, firstparent, secondparent, SIMPLIFY=FALSE)
  return(newcouple)
}



getrankfitness <- function(fitness, reverse = TRUE){
  #' calculate rank-based fitness
  #'
  #' getrankfitness calculates the fitness based on the objective function's rank
  #'
  #' @author Lei Zhang
  #' @param fitness goodness of fit ~ a casenum * 1 vector , default is AICvalue
  #' @param reverse whether we need to reverse the order when we rank the values in fitness, depends on whether the larger the value in the "fitness" vector , the better the fitness
  #' @example getrankfitness(fitness = c(1,2,3,4,-10,7), reverse = TRUE); getrankfitness(fitness = c(1,2,3,4,-10,7), reverse = FALSE)

  stopifnot(is.vector(fitness))
  stopifnot(is.logical(reverse))

  rank <- rank( ((-1)*reverse + 1 *(1 - reverse)) * fitness )
  rankfit <- 2 * rank/(length(fitness) * (length(fitness)+1))
  return(rankfit)
}



selectparents <- function(originalparents, couplenum, method, fitness, subsetnum = NULL ){
  #' select parents in the genetic algorithms
  #'
  #' In this selection mechanism, parents are chosen to produce offspring in the next stage.
  #' Three different methods are provided: "twopropselection", "onepropselection" and "tournament"
  #' @return Returns the selected parents ~ list
  #' @author Lei Zhang
  #' @param originalparents is a casenum * variablenum matrix
  #' @param couplenum number of couples selected
  #' @param method the method to select the parents, "twopropselection", "onepropselection" or "tournament"
  #' @param fitness goodness of fit ~ a casenum * 1 vector , default is AICvalue
  #' @param subsetnum In method "tournament", we partition the set of chromosomes in generation t into "subsetnum" disjoint subsets
  #' @example selectparents(originalparents = matrix(rep(c(1,0,0,1),5), nrow = 5), couplenum = 10, method = "tournament", fitness=c(1,4,-2,7,5), subsetnum = 2)

  stopifnot(is.matrix(originalparents))
  stopifnot(is.numeric(couplenum))
  stopifnot(is.character(method))
  stopifnot(is.vector(fitness))

  fitness <- getrankfitness(fitness = fitness)
  switch(method,
         twopropselection = twopropselection(originalparents, couplenum, fitness),
         onepropselection = onepropselection(originalparents, couplenum, fitness),
         tournament = tournament(originalparents, couplenum, subsetnum, fitness))
}

