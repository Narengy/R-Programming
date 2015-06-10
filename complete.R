complete <- function(directory, id = 1:332) 
  {
  files = dir(directory, pattern = "", full.names = TRUE, ignore.case = TRUE)
  nobs=numeric()
  
  
  for (i in id){
     data = read.csv(files[i])
     nobs = c(nobs, sum(complete.cases(data)))
  }
     
  
    return(data.frame(id,nobs))

}
  

  

