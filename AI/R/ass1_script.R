# Install the package
# install.packages("DeliveryMan_1.1.0.tar.gz", repos = NULL, type="source")

# Load the library
library("DeliveryMan")

# Read documentation
# ?runDeliveryman
# ?testDM

myFunctionAss <- function(trafficMatrix, carInfo, packageMatrix) {
  print(packageMatrix)
  print(trafficMatrix)
  mdToNextPack = 100
  # om bilen inte har ett paket
  if(carInfo$load == 0) {
    # gå igenom paketen på planen, och hitta det närmsta
    for(i in 1:nrow(packageMatrix)) {
      distanceToPackage <- calcMD(carInfo$x, carInfo$y, packageMatrix[i,1], packageMatrix[i,2])
      print(distanceToPackage)
      if (distanceToPackage < mdToNextPack && packageMatrix[i,5] == 0) {
        mdToNextPack = distanceToPackage
        nextPackCoord <- c(packageMatrix[i,1], packageMatrix[i,2])
      }
    }
    print(mdToNextPack)
    print(nextPackCoord)
    carInfo$nextMove <- (a_star(trafficMatrix, nextPackCoord, calcHeurMatrix(nextPackCoord, trafficMatrix), carInfo, packageMatrix))
    return(carInfo)
  }
  else if (carInfo$load != 0) {
    print(carInfo)
    for(i in 1:nrow(packageMatrix)) {
      if (packageMatrix[i,5] == 1) {
        goalCoord <- c(packageMatrix[i,3], packageMatrix[i,4])
        carInfo$nextMove <- (a_star(trafficMatrix, goalCoord, calcHeurMatrix(goalCoord, trafficMatrix), carInfo, packageMatrix))
        return(carInfo)
      }
    }
  }
}

a_star <- function(trafficMatrix, goalCoord, heurMatrix, carInfo, packageMatrix) {
  frontier = list()
  priorityQ = list()
  bestWay = 100
  bestDir = 0
  if (goalCoord[[1]] == carInfo$x && goalCoord[[2]] == carInfo$y) {
    print("hellO!")
    bestDir = 5
    return(bestDir)
  }
    neighborUp = list(dir = 8, x = carInfo$x, y = carInfo$y + 1, f = heurMatrix[carInfo$x, carInfo$y+1] + trafficMatrix$vroads[carInfo$x, carInfo$y])
    neighborDown = list(dir = 2, x = carInfo$x, y = carInfo$y - 1, f = heurMatrix[carInfo$x, carInfo$y-1] + trafficMatrix$vroads[carInfo$x, carInfo$y-1])
    neighborRight = list(dir = 6, x = carInfo$x + 1, y = carInfo$y, f = heurMatrix[carInfo$x+1, carInfo$y] + trafficMatrix$hroads[carInfo$x, carInfo$y])
    neighborLeft = list(dir = 4, x = carInfo$x - 1, y = carInfo$y, f = heurMatrix[carInfo$x-1, carInfo$y] + trafficMatrix$hroads[carInfo$x-1, carInfo$y])
    neighborList = list(neighborUp, neighborDown, neighborRight, neighborLeft)
    print(neighborList)
    for(i in 1:length(neighborList)) {
      if(neighborList[[i]]$x != 0 && neighborList[[i]]$y != 0){
        if(neighborList[[i]]$f < bestWay){
          bestWay = neighborList[[i]]$f
          bestDir = neighborList[[i]]$dir
        }
      }
    }
  print(bestDir)
  return(bestDir)
}

# function som beräknar heur-avstånd från alla noder till målet
calcHeurMatrix <- function(goalCoord, trafficMatrix) {
  heurMat = matrix(nrow = nrow(trafficMatrix$vroads), ncol = ncol(trafficMatrix$hroads))
  for (x in 1:ncol(trafficMatrix$hroads)) {
    for (y in 1:nrow(trafficMatrix$vroads)) {
      heurMat[x,y] = calcMD(x,y,goalCoord[[1]], goalCoord[[2]])
    }
  }
  print(heurMat)
  return(heurMat)
}

# Calculates manhattan distance
calcMD <- function(startPosX, startPosY, goalX, goalY) {
  goalXY <- c(goalX, goalY)
  carXY <- c(startPosX, startPosY)
  dis <- rbind(goalXY,carXY)
  dist <- dist(dis, method= "manhattan")
  manDist = as.matrix(dist)[2,1]
  return(manDist)
}

