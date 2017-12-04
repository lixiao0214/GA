crossover_onesplit<-function(parents){
  new<-generation<-list()
  ###calculate the length of chromosomes C
  n<-length(parents[[1]][1,])
  
  ###get offspring
  for(i in 1:n){
    
    ###store parents in two vector
    parents_1<-parents[[i]][1,]
    parents_2<-parents[[i]][2,]
    
    ###randomly yield index of point to crossover
    split_index<-sample(1:n,1)
    
    ###crossover and get two offsprings
    offspring_1<-c(parents_1[1:split_index],parents_2[split_index+1:n])
    offspring_2<-c(parents_2[1:split_index],parents_1[split_index+1:n])

    new_generation[[i]]<-rbind(offspring_1,offspring_2)
  }
  
  return(new_generation)
}