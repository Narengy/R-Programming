corr <- function(directory, threshold=0) {
  files = dir(directory, pattern = "", full.names = TRUE, ignore.case = TRUE)
  id=1:332
  corrvector=c()
  for (i in id) {
    data = read.csv(files[i])
    x=data[complete.cases(data),2:3]
    x1=length(x)
    x1=sum(complete.cases(x))
    if (x1 > threshold) {
      correlation=cor(x$sulfate, x$nitrate)
      corrvector=c(corrvector,correlation)          
    }
  }        
  corrvector
}
