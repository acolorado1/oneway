data(mtcars)

#is three a relationship between the number of cylinders and
# fuel efficeincy

table(mtcars$cyl)

stats <- function(x){
  c(n = length(x), mean = mean(x), sd = sd(x))
}

result <- aggregate(mpg~cyl, data = mtcars, stats)


method<-function(data, factor){
  mean <- aggregate(data, list(factor), FUN=mean)
  count <- aggregate(data, list(factor), FUN=length)
  sd <- aggregate(data, list(factor), FUN=sd)
  output<-merge(mean, count, by = "Group.1")
  output<-merge(output,sd, by = "Group.1")
  colnames(output)<-c("cyl", "mean", "count", "sd")

  print(output)
}
