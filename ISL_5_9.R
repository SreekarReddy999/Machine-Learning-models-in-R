library(ISLR2)
info = Boston
head(info)
mean(info$medv)
spread.sd = sd(info$medv)
spread.sem = spread.sd / sqrt(dim(info)[1])
spread.sem
set.seed(312)
beat.fn = function(data, index){
  return(mean(data[index, 'medv']))
}
library(boot)
boot(info, beat.fn, 10000)
t.test(info$medv)
median(info$medv)
set.seed(312)
beat.fn = function(data, index){
  return(median(data[index, 'medv']))
}
boot(info, beat.fn, 10000)
quantile(info$medv, c(0.1))
set.seed(312)
beat.fn = function(data, index){
  return(quantile(data[index, 'medv'], 0.1))
}
boot(info, beat.fn, 10000)