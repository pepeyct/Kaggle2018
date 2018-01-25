library(data.table)
library(dplyr)
b.trian=fread("C:/Users/CZhao/beluga_train.csv")
start_end_location= b.trian[,.(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude)]
#toy = start_end_location[1:4,]
######### define function google map ############
gmap_fun=function(X){ 
             X = as.vector(X)
             origin = paste0(X[1,2],"+",X[1,1])
             destination = paste0(X[1,4],"+",X[1,3])
             results = gmapsdistance(origin, destination, mode = "driving", shape = "long")
             c(results$Time,results$Distance)
}
#
gmap_fun=function(X){ 
  X = as.vector(X)
  origin = paste0(X[2],"+",X[1])
  destination = paste0(X[4],"+",X[3])
  results = gmapsdistance(origin, destination, mode = "driving", shape = "long")
  c(results$Time,results$Distance)
}
######### define function google map ############

train1 = mutate(b.trian,time=apply(start_end_location,1,gmap_fun))
set.api.key( "AIzaSyAHuQU5y8vspdJ7B7aQ7c4j1mDe122JeJk")
g.result  = apply(start_end_location,1,gmap_fun)
result  = matrix(NA, nrow = 1458644,ncol=2)

for ( i in 2211 : 1458644){
  tryCatch({
  set.api.key( "AIzaSyAHuQU5y8vspdJ7B7aQ7c4j1mDe122JeJk")
  X = start_end_location[i,]
  origin = paste0(X[1,2],"+",X[1,1])
  destination = paste0(X[1,4],"+",X[1,3])
  results = gmapsdistance(origin, destination, mode = "driving", shape = "long")
  print(i)
  result[i,] = c(results$Time,results$Distance)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

X=as.numeric(X)
google_distance(origins = toy[,2:1], destinations =toy[,4:3],key= "AIzaSyAHuQU5y8vspdJ7B7aQ7c4j1mDe122JeJk")
