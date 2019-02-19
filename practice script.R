data("mtcars")
len = length(mtcars$mpg)
print(len)
mu = aggregate(mtcars$hp ~ mtcars$am, FUN=mean)
print(mu)
anova = aov(hp ~ am, data = mtcars)
print(summary(anova))
ancova = aov(hp ~ am + qsec + am*qsec, data = mtcars)
print(summary(ancova))
plot(mtcars$hp, mtcars$am + mtcars$qsec)
print(summary(lm(mtcars$hp ~ mtcars$am + mtcars$qsec)))

k =  numeric()
for(i in 1:200){
  k[i] = rnorm(1)
}
print(k)
hist(k)


l = list()
for(i in 1:200){
  l[[i]] = rnorm(1)
}
print(length(l))

