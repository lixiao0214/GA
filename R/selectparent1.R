
###calculate the fitness, the default is AIC
# getfitness <- function(originalparents, fitfun = NULL){
#   #originalparents is a casenum * variablenum matrix
#   #fitness is a casenum * 1 vector , usually it's AICvalue
#   #fitfun should be a function whose input is originalparents(a casenum * variablenum matrix)
#   #and its output should be a casenum * 1 vector
#   
#   if (fitfun = NULL){
#     #call the AIC function to calculate the AIC value of the originalparents
#     fitnessvalue <- AIC(originalparents)
#   }
#   else{
#     fitnessvalue <- fitfun(originalparents)
#     
#   }
#   return(fitnessvalue)
# }

# 
# ###some examples of fitfun other than AIC
# AICrankfit <- function(originalparents){
#   AICvalue <- AIC(originalparents)
#   AICrank <- rank(AICvalue)
#   AICrankfit <- 2 * AICrank/(nrow(originalparents) * (nrow(originalparents)+1))
#   return(AICrankfit)
# }



###method one : select one parent with probability proportional to fitness(or fitnessrank)
###and to select the other parent completely at random
twopropselection <- function(originalparents, couplenum, fitness){
  fitness <- exp(fitness)/sum(exp(fitness))
  firstparent <- originalparents[sample(1:nrow(originalparents), size = couplenum, replace = TRUE,                                       prob = fitness),]
  secondparent <- originalparents[sample(1:nrow(originalparents), size = couplenum, replace = TRUE),]
  #transform the each row of the matrix into a list
  firstparent <- split(firstparent, row(firstparent)) 
  secondparent <- split(secondparent, row(secondparent))
  #combine firstparent and secondparent to form the list of paired parents
  newcouple <- mapply(rbind, firstparent, secondparent, SIMPLIFY=FALSE)
  return(newcouple)
}


###method two:select each parent independently with probability proportional to fitness
onepropselection <- function(originalparents, couplenum, fitness){
  fitness <- exp(fitness)/sum(exp(fitness))
  firstparent <- originalparents[sample(1:nrow(originalparents), size = couplenum, replace = TRUE, prob = fitness),]
  secondparent <- originalparents[sample(1:nrow(originalparents), size = couplenum, replace = TRUE, prob = fitness),]
  #transform the each row of the matrix into a list
  firstparent <- split(firstparent, row(firstparent)) 
  secondparent <- split(secondparent, row(secondparent))
  #combine firstparent and secondparent to form the list of paired parents
  newcouple <- mapply(rbind, firstparent, secondparent, SIMPLIFY=FALSE)
  return(newcouple)
}


###method three:tournament selection
###the set of parents in generation t is randomly partitioned into k disjoint
###subsets of equal size(perhaps with a few remaining parents temporarily ignored)
###The best individual in each group is chosen as a parent.
###Additional random partitionings are carried out until sufficient parents have
###been generated.Parents are then paired randomly for breeding. 
tournament <- function(originalparents, couplenum, subsetnum, fitness){
  fitness <- exp(fitness)/sum(exp(fitness))
  partitiontime <- ceiling(couplenum*2/subsetnum)
  parentspool <- matrix(0, partitiontime*subsetnum , ncol(originalparents))
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



###Select parents based on the parameters
selectparents <- function(originalparents, couplenum, method, fitness, subsetnum = NULL ){
  #originalparents is a casenum * variablenum matrix
  #fitness is a casenum * 1 vector , usually it's AICvalue
  #method is the method to select the parents
  
  #fitness <- getfitness(originalparents, fitfun = fitfun)
  switch(method, 
         twopropselection = twopropselection(originalparents, couplenum, fitness),
         onepropselection = onepropselection(originalparents, couplenum, fitness),
         tournament = tournament(originalparents, couplenum, subsetnum, fitness))
}






