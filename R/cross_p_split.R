crossover_p_split <- function(parents, p){
  new_generation<-c()
  # Calculate the length of chromosomes C, and the numebr of pairs of parents P
  C<-length(parents[[1]][1,]);P<-length(parents)

  for(i in 1:P){

    ###store offsprings
    offspring_1<-c();offspring_2<-c()

    ###store parents in two vector
    parents_1<-parents[[i]][1,];parents_2<-parents[[i]][2,]

    ###randomly yield index of point to crossover
    split_index<-sort(sample(2:C,p))
    split_index<-c(1,split_index,(C+1))
    ###split parents' chromosomes into p+1 pieces, and store in two lists
    pieces_1<-list();pieces_2<-list()
    for(j in 1:(p+1)){
      pieces_1[[j]]<-parents_1[split_index[j]:(split_index[j+1]-1)]
      pieces_2[[j]]<-parents_2[split_index[j]:(split_index[j+1]-1)]
    }

    ###choose a half of pieces from each parent and get offsprings
    chosen_pieces_1<-sort(sample(1:(p+1),round(p/2)+1))

    for(j in 1:(p+1)){

      ###judge whose jth pieces genes should be inherited
      if(sum(chosen_pieces_1==j)==1) {
        offspring_1<-c(offspring_1,pieces_1[[j]]);offspring_2<-c(offspring_2,pieces_2[[j]])
      }
      else{
        offspring_1<-c(offspring_1,pieces_2[[j]]);offspring_2<-c(offspring_2,pieces_1[[j]])
      }
    }

    ###crossover and get two offsprings
    new_generation<-rbind(new_generation,offspring_1,offspring_2)

  }
  row.names(new_generation)<-c(1:(2*P))
  return(new_generation)
}
