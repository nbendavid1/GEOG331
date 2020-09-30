#set function to evaluate truth of statement
assert <- function(statement,err.message){
  if(statement == FALSE){
    print(err.message)
  }}

#test with false statement, should return error
assert(1 == 2, "error: unequal values")

#test with true statement, should not return error
assert(2 == 2, "error: unequal values")

#read in the data file, skipping first 3 rows, specifying that the NA is designated differently
datW <- read.csv("/Users/nadav/Documents/GitHub/data_import_GEOG331/bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
#preview data
print(datW[1,])