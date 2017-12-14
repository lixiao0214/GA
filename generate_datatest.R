generate_testdata<-function(P,C){
  data<-list()
  for(i in 1:P){
    data[[i]]<-rbind(sample(c(0,1),C,replace=TRUE),sample(c(0,1),C,replace = TRUE))
  }
  return(data)
}

lapply(1:100, f<-function(i){
  data<-rbind(rbinom(n = 10, size = 1, prob = runif(n = 1, min = 0, max = 1)),
              rbinom(n = 10, size = 1, prob = runif(n = 1, min = 0, max = 1)))
})


population<-rbind(rbinom(n = 9, size = 1, prob = runif(n = 1, min = 0, max = 1)),
            rbinom(n = 9, size = 1, prob = runif(n = 1, min = 0, max = 1)))

data<-as.data.frame(matrix(1:100,10,10))
data
