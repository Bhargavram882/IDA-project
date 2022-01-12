library(tidyverse)
library(data.tree)


setwd("C:\\Users\\DELL\\OneDrive\\Desktop\\DATA SCIENCE")


d3 <- as.data.frame(read_csv("student-por.csv"))

set.seed(1235)





d3$absences <- ifelse( d3$absences > 10 ,ifelse( d3$absences < 26 , 10, 25) , 0)

d3$G1 <- ifelse( d3$G1 > 5 ,ifelse( d3$G1 < 12 , 11, ifelse( d3$G1 < 17 ,16, 20)) , 0)

d3$G2 <- ifelse( d3$G2 > 5 ,ifelse( d3$G2 < 12 , 11, ifelse( d3$G2 < 17 ,16, 20)) , 0)

d3$G3 <- ifelse( d3$G3 > 12, 1 , 0)




d3[, c(1:33)] <- sapply(d3[, c(1:33)], as.character )



d4 <- data.frame(lapply(d3, as.factor))


d4[, c(1:33)] <- sapply(d4[, c(1:33)], as.character )

set.seed(12345)



#bootstrapping

train <- sample(1:nrow(d4),size = nrow(d4),replace = TRUE)
bstrap  = d4[train,] 

train <- sample(1:nrow(bstrap),size = ceiling(0.67*nrow(bstrap)),replace = FALSE)
# training set
d4_train <- bstrap[train,]
# test set
d4_test <- bstrap[-train,]




IsPure <- function(data) {
  length(unique(data[,ncol(data)])) == 1
}

Entropy <- function( vls ) {
  res <- vls/sum(vls) * log2(vls/sum(vls))
  res[vls == 0] <- 0
  -sum(res)
}

InformationGain <- function( tble ) {
  tble <- as.data.frame.matrix(tble)
  entropyBefore <- Entropy(colSums(tble))
  s <- rowSums(tble)
  entropyAfter <- sum (s / sum(s) * apply(tble, MARGIN = 1, FUN = Entropy ))
  informationGain <- entropyBefore - entropyAfter
  return (informationGain)
}






# ID3 ALGO
TrainID3 <- function(node, data) {
  
  node$obsCount <- nrow(data)
  
  #if the data-set is pure (e.g. all toxic), then
  if (IsPure(data)) {
    #construct a leaf having the name of the pure feature (e.g. 'toxic')
    child <- node$AddChild(unique(data[,ncol(data)]))
    node$feature <- tail(names(data), 1)
    child$obsCount <- nrow(data)
    child$feature <- ''
  } else {
    #chose the feature with the highest information gain (e.g. 'color')
    ig <- sapply(colnames(data)[-ncol(data)], 
                 function(x) InformationGain(
                   table(data[,x], data[,ncol(data)])
                 )
    )
    feature <- names(ig)[ig == max(ig)][1]
    
    node$feature <- feature
    
    #take the subset of the data-set having that feature value
    childObs <- split(data[,!(names(data) %in% feature)], data[,feature], drop = TRUE)
    
    for(i in 1:length(childObs)) {
      #construct a child having the name of that feature value (e.g. 'red')
      child <- node$AddChild(names(childObs)[i])
      
      #call the algorithm recursively on the child and the subset      
      TrainID3(child, childObs[[i]])
    }
    
  }
  
  
  
}

tree <- Node$new("student")
TrainID3(tree, d4_train )
print(tree, "feature", "obsCount")



Predict <- function(tree, features) {
  if (tree$children[[1]]$isLeaf) return (tree$children[[1]]$name)
  child <- tree$children[[features[[tree$feature]]]]
  return ( Predict(child, features))
}








actual <- d4_test$G3



predicted <- c()




itr <- 1:nrow(d4_test)
for( i in  itr )
  predicted = append(predicted,  Predict(tree,subset(d4_test[i,], select = -c(G3) ))  )


TP = 0  
FP = 0 
FN = 0
TN = 0



itr <- 1:length(predicted)
for( i in  itr ) { 
    if(predicted[i] == actual[i] && predicted[i] == 1  )
      TP = TP + 1 
    else if(predicted[i] == actual[i] && predicted[i] == 0  )
      TN = TN + 1
    else if( actual[i] == 0 && predicted[i] == 1  )
      FP = FP + 1
    else 
      FN = FN + 1 
}


precision = TP/(TP+FP)
recall =  TP/(TP+FN) 
accuracy = TP+TN/(TP+TN+FP+FN) 
F1 = 2*((precision*recall)/precision+recall)


CM <- matrix( c(TP,FP,FN,TN) ,nrow = 2, byrow = TRUE )

sprintf("precision = %f", precision)
sprintf("recall = %f", recall)
sprintf("accuracy = %f", accuracy)
sprintf("F1 score = %f", F1)

print ("confusion matrix") 
print(CM)


