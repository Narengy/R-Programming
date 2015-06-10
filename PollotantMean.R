pollutantmean <- function(directory, pollutant, id = 1:332)
{
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  files = dir(directory, pattern = "", full.names = TRUE, ignore.case = TRUE)
  #cat(id,"\n")
  totalData = c()
  for (i in id) {
    #cat(i,"\n")
    data = read.csv(files[i])
    
    data1=data[pollutant]
    data1=data1[!is.na(data1)]
    totalData=c(totalData,data1)
  }
  mean(totalData)
}