# Install the package
# install.packages("DeliveryMan_1.1.0.tar.gz", repos = NULL, type="source")

# Load the library
library("DeliveryMan")

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
    if(carInfo$x == nextPackCoord[[1]] && carInfo$y == nextPackCoord[[2]]) {
      carInfo$nextMove = 5
      return(carInfo)
    }
    print(mdToNextPack)
    print(nextPackCoord)
    frontier <- list(list(currPos = list(x = carInfo$x, y = carInfo$y), currCost = 0, pathSearched = list()))
    spaceTheFinalFrontier <- a_star(frontier, trafficMatrix, nextPackCoord)
    if(spaceTheFinalFrontier[[1]]$searchedPath[[1]]$y < carInfo$y){
      carInfo$nextMove = 2
    }
    if(spaceTheFinalFrontier[[1]]$searchedPath[[1]]$y > carInfo$y){
      carInfo$nextMove = 8
    }
    if(spaceTheFinalFrontier[[1]]$searchedPath[[1]]$x < carInfo$x){
      carInfo$nextMove = 4
    }
    if(spaceTheFinalFrontier[[1]]$searchedPath[[1]]$x > carInfo$x){
      carInfo$nextMove = 6
    }
    if(carInfo$x == nextPackCoord[[1]] && carInfo$y == nextPackCoord[[1]])
    return(carInfo)
  }
  else if (carInfo$load != 0) {
    print(carInfo)
    for(i in 1:nrow(packageMatrix)) {
      if (packageMatrix[i,5] == 1) {
        goalCoord <- c(packageMatrix[i,3], packageMatrix[i,4])
        frontier <- list(list(currPos = list(x = carInfo$x, y = carInfo$y), currCost = 0, pathSearched = list()))
        spaceTheFinalFrontier <- a_star(frontier, trafficMatrix, goalCoord)
        if(spaceTheFinalFrontier[[1]]$searchedPath[[1]]$y < carInfo$y){
          carInfo$nextMove = 2
        }
        if(spaceTheFinalFrontier[[1]]$searchedPath[[1]]$y > carInfo$y){
          carInfo$nextMove = 8
        }
        if(spaceTheFinalFrontier[[1]]$searchedPath[[1]]$x < carInfo$x){
          carInfo$nextMove = 4
        }
        if(spaceTheFinalFrontier[[1]]$searchedPath[[1]]$x > carInfo$x){
          carInfo$nextMove = 6
        }
        if(carInfo$x == nextPackCoord[[1]] && carInfo$y == nextPackCoord[[1]])
          return(carInfo)
      }
    }
  }
}

a_star <- function(frontier, trafficMatrix, goalCoord) {
  priorityQ = list()
  bestWay = 100
  bestDir = 0
  if (goalCoord[[1]] == frontier[[1]]$currPos$x) {
    if(goalCoord[[2]] == frontier[[1]]$currPos$y){
      return(frontier) 
    }
  }
  expPoint = frontier[[1]]$currPos
  prevCost = frontier[[1]]$currCost
  pathSearched = frontier[[1]]$pathSearched
  heurMatrix = calcHeurMatrix(goalCoord, trafficMatrix)
  frontier[[1]] <- NULL
  if(expPoint$x < 10 && expPoint$y < 10){
      frontier = expand(expPoint, list(x = expPoint$x, y = expPoint$y + 1), frontier, heurMatrix, trafficMatrix$vroads[expPoint$x, expPoint$y], prevCost, pathSearched)
      frontier = expand(expPoint, list(x = expPoint$x, y = expPoint$y - 1), frontier, heurMatrix, trafficMatrix$vroads[expPoint$x, expPoint$y-1], prevCost, pathSearched)
      frontier = expand(expPoint, list(x = expPoint$x + 1, y = expPoint$y), frontier,  heurMatrix, trafficMatrix$hroads[expPoint$x, expPoint$y], prevCost, pathSearched)
      frontier = expand(expPoint, list(x = expPoint$x - 1, y = expPoint$y), frontier,  heurMatrix, trafficMatrix$hroads[expPoint$x-1, expPoint$y], prevCost, pathSearched)
  }
  if(expPoint$x == 10){
    frontier = expand(expPoint, list(x = expPoint$x, y = expPoint$y + 1), frontier, heurMatrix, trafficMatrix$vroads[expPoint$x, expPoint$y], prevCost, patchSearched)
    frontier = expand(expPoint, list(x = expPoint$x, y = expPoint$y - 1), frontier, heurMatrix, trafficMatrix$vroads[expPoint$x, expPoint$y-1], prevCost, patchSearched)
  }
  if(expPoint$y == 10){
    frontier = expand(expPoint, list(x = expPoint$x + 1, y = expPoint$y), frontier,  heurMatrix, trafficMatrix$hroads[expPoint$x, expPoint$y], prevCost, patchSearched)
    frontier = expand(expPoint, list(x = expPoint$x - 1, y = expPoint$y), frontier,  heurMatrix, trafficMatrix$hroads[expPoint$x-1, expPoint$y], prevCost, patchSearched)
  }
  return(a_star(frontier, trafficMatrix, goalCoord))
}

expand <- function(expPoint, addPoint, frontier, heurMatrix, cost, prevCost, pathSearched) {
  if(addPoint$x > 0) {
    if(addPoint$x < 11) {
      if(addPoint$y > 0){
        if(addPoint$y < 11){
          newCost = cost + prevCost
          newFrontierEl = list(list(currPos = addPoint, currCost = newCost, pathSearched = append(pathSearched, list(expPoint))))
          for(i in 1:length(frontier)){
            if(frontier[[i]]$currCost + heurMatrix[frontier[[i]]$currPos$x, frontier[[i]]$currPos$y] < newCost){
              frontier = append(frontier, list(newFrontierEl), after = 1-i)
            }
          }
        }
        return(frontier)
      }
      return(frontier)
    }
    return(frontier)
  }
  return(frontier)
}

# function som beräknar heur-avstånd från alla noder till målet
calcHeurMatrix <- function(goalCoord, trafficMatrix) {
  heurMat = matrix(nrow = nrow(trafficMatrix$vroads), ncol = ncol(trafficMatrix$hroads))
  for (x in 1:ncol(trafficMatrix$hroads)) {
    for (y in 1:nrow(trafficMatrix$vroads)) {
      heurMat[x,y] = calcMD(x,y,goalCoord[[1]], goalCoord[[2]])
    }
  }
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

