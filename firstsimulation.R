permTest = function(group1, group2){
  hypo = mean(group1) - mean(group2)
  hypoAbs = abs(hypo)
  allValues  <- c(group1,group2)
  permutations = 1000
  
  lists = c()
  
  groupMemberShips <- c(rep(TRUE,length(group1)),rep(FALSE,length(group2)))
  hypothetical =rep(NA,permutations)
  for (i in 1:permutations){
    curGroupMembersShips =sample(groupMemberShips)
    curGroup1= allValues[curGroupMembersShips]
    curGroup2=allValues[!curGroupMembersShips]
    means = mean(curGroup1) - mean(curGroup2)
    absolute = abs(means)
    lists = c(lists, absolute)
  }
  
  for (j in lists){
    if (hypo > j){}
  }

return(sum(lists<=hypo)/permutations)
}


pvalue = c()
for (i in 1:2){
  set.seed(i)
  x = rnorm(10, mean = 1)
  y = rnorm(10)
  test = t.test(x, y)
  p_value = test$p.value
  pValues = c(pValues, p_value)
  print(test$mean)
  
  permTest(x,y)
  
}

# pvals = numeric()
# for(i in 1:1000){
#  x = rnorm(n = 100, mean = 0, sd = 1)
#  res = t.test(x, mu = 0)
#  pvals[i] = res$p.value
# }

# hist(pvals)
# abline(v=0.05, col="red", lwd=3)

  
  
  
  
  