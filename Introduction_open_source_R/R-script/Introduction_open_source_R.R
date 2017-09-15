##############################################################################
##############################################################################
##                            R Script for                                  ##
##        Introduction to open source Spatial Analysis Tools and R          ##
##                        by Avit Kumar Bhowmik                             ##
##############################################################################
##############################################################################

## After completion of the brain warming
## Let's do some primary school math

1+1 

## Create two vectors of one element and do the same as above

a = 1
b = 2
a+b

## With longer numeric vectors 

C <- c(1, 2, 3, 4, 5)
D <- 6:10
C+D
E <- D+1

## Look inside a vector

E

## Extract an element from a vector

E[3]

## Positional query in a vector

which(E==9)

## Character vectors

F <- c("a", "b", "c", "d", "e")
G <- letters[6:10]

## Create a matrix

?cbind
Matrix <- cbind(C, D, E, F, G)
class(Matrix)
class(Matrix[,1])
class(C)
class(F)

## This did not work because of different vector types
## Therefore, create matrices with the same vector types

Matrix_int <- cbind(C, D, E)
Matrix_char <- cbind(F, G)

## However, we can combine them in a data frame

Data_Frame <- data.frame(C, D, E, F, G, options(stringsAsFactors = FALSE))
class(Data_Frame$C)
class(Data_Frame[,5])

## Differences between matrix and data frame

class(Matrix_int[1,])
class(Data_Frame[1,])
class(Data_Frame$D)

## Coercion vector types

Data_Frame$D <- as.numeric(Data_Frame$D)

## Create and enter a list

List <- list(Matrix_int, Matrix_char, Data_Frame)
List[[2]]

## Task 1: In the list object, add one to each of the element of the numeric matrix
## and replace the existing numeric matrix with the new numeric matrix.

List[[1]] <- List[[1]]+1


## Conditional elements: If Else
## If one of the vectors in the Data Frame is numeric, we want to add 1 to all of
## its elements, otheriwse the vector will remain as it is

if(class(Data_Frame$G)=="numeric"){Data_Frame$G+1}else{Data_Frame$G}
if(class(Data_Frame$C)=="numeric"){Data_Frame$C+1}else{Data_Frame$C}


## Loops
## For all the vectors in the Data Frame, if a vector and its subsequent vector
## are numeric, we will add each vector to its subsequent vector and replace
## the existing vector; otherwise the vector will remain as it is.

for(i in 1:(ncol(Data_Frame)-1)){
if(class(Data_Frame[,i])=="numeric" & class(Data_Frame[,i+1])=="numeric")
{Data_Frame[,i] <- Data_Frame[,i]+Data_Frame[,i+1]}else{Data_Frame[,i] <- Data_Frame[,i]}
}

## Task 2: For all the vectors in the Data Frame, if a vector and its precedent vector
## are numeric, we will deduct corresponding precedent vector from each vector and replace
## the existing vector; otherwise the vector will remain as it is.

for(i in 2:ncol(Data_Frame)){
  if(class(Data_Frame[,i])=="numeric" & class(Data_Frame[,i-1])=="numeric")
  {Data_Frame[,i] <- Data_Frame[,i]-Data_Frame[,i-1]}else{Data_Frame[,i] <- Data_Frame[,i]}
}

## Alternative for loops, i.e. apply(), lapply, sapply, ddply
## The first loop with sapply function

NDT <- sapply(1:(ncol(Data_Frame)-1),
      function(x) if(class(Data_Frame[,x])=="numeric" & class(Data_Frame[,x+1])=="numeric")
      {Data_Frame[,x] <- Data_Frame[,x]+Data_Frame[,x+1]}else{Data_Frame[,x] <- Data_Frame[,x]})

## Be aware that the output is coereced to a matix and all vectors are character

#############################################
##         Spatial data in R               ##
#############################################

## Install a package, don't run in the university computers

# install.packages("sp")

## Initiate a package

library(sp)

## or require(sp)

## Create a point shape file, i.e. SpatialPointsDataFrame
## Create coordinate matrix and data for a point shape file. WGS coordinate limits for Germany:
## Longitude: 47 to 55 degree N, and Latitude: 5 to 16 degree E

coords <- cbind(x = sample(47:55, size = 8), y = sample(5:16, size = 8))
data <- data.frame(a=1:8, b=9:16, c=17:24)

## Combine into a SpatialPointsDataFrame

SPD <- SpatialPointsDataFrame(coords=coords, data=data)
class(SPD)

## Plot a SpatialPointsDataFrame

plot(SPD)
spplot(SPD, colorkey=T, scales=list(draw=T))

## Write and read a shapefile

library(maptools)

## Set a working directory

setwd("/Users/avitbhowmik/Teaching_Supervision/Teaching_Landau/Summer_academy_DAAD/September_2016/summer_academy_16/Introduction_open_source_R/data")

## Write the SpatialPointsDataFrame to a shape file

writePointsShape(SPD, "SPD")

## Read a shapefile from the directory

SPD_sp <- readShapePoints("SPD")
class(SPD_sp)

##############################################################################
##                                    End                                   ##
##############################################################################
