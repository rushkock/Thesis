library(coin)
absoluteMeanDifferences <- function(group1,group2){
  return(abs(mean(group1)-mean(group2)))
}

permTest <- function(group1,group2,permutations=1000){
  testStatistic <- absoluteMeanDifferences
  observed <- testStatistic(group1,group2)
  
  allValues <- c(group1,group2)
  groupMemberShips <- c(rep(TRUE,length(group1)),rep(FALSE,length(group2)))
  hypothetical <- rep(NA,permutations)
  for (i in 1:permutations){
    curGroupMembersShips <- sample(groupMemberShips)
    curGroup1 <- allValues[curGroupMembersShips]
    curGroup2 <- allValues[!curGroupMembersShips]
    hypothetical[i] <- testStatistic(curGroup1,curGroup2)
  }
  return(sum(observed<=hypothetical)/permutations)
}

typeIerrorsTTest <- 0
typeIerrorsPermut <- 0
reps <- 1000
mean2 <- 20
mean1 = 21
typeIerrorsTTest = 0
typeIerrorsPermut = 0
typeIIerrorsTTest = 0
typeIIerrorsPermut = 0
for (i in 1:reps){ #repeat 1000 times
  ##generate data according to normal
  group1 <- rnorm(n=20,mean=mean2,sd=1)
  group2 <- rnorm(n=20,mean=mean1,sd=1)
  group1 = rlnorm(n=20, meanlog = mean2, sdlog = 1)
  group2 = rlnorm(n=20, meanlog = mean1, sdlog = 1)
  test = t.test(group1,group2)
  pvalue = test$p.value
  #get p-value from t-test
  
  permPValue = permTest(group1, group2)
  #get p-value from permutation test
  if (abs(mean1 - mean2) == 0){
    if (pvalue < 0.05 ){
      typeIerrorsTTest = typeIerrorsTTest + 1
    }
    
    if (permPValue < 0.05){
      typeIerrorsPermut =  typeIerrorsPermut + 1
    }
  }
  else{
    if (pvalue > 0.05 ){
      typeIIerrorsTTest = typeIIerrorsTTest + 1
    }
    
    if (permPValue > 0.05){
      typeIIerrorsPermut =  typeIIerrorsPermut + 1
    }
  }
  
  #check whether type I error for each method (no mean difference but test concludes difference)
  #if yes, increase the respective counter

} #end of for
cat('t-test\n')
cat(sprintf('Type I error rate: %.2f\n',typeIerrorsTTest/reps))
cat(sprintf('Type II error rate: %.2f\n',typeIIerrorsTTest/reps))
cat('Permutation-test\n')
cat(sprintf('Type I error rate: %.2f\n',typeIerrorsPermut/reps))
cat(sprintf('Type II error rate: %.2f\n',typeIIerrorsPermut/reps))
