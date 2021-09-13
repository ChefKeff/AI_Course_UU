library("DeliveryMan")
myFunctionAss <- function(trafficMatrix, carInfo, packageMatrix) {
  mdToNextPack = 100
  if(carInfo$load == 0) {
    for(i in 1:nrow(packageMatrix)) {
      distanceToPackage <- calcMD(carInfo$x, carInfo$y, packageMatrix[i,1], packageMatrix[i,2])
      if (distanceToPackage < mdToNextPack && packageMatrix[i,5] == 0) {
        mdToNextPack = distanceToPackage
        nextPackCoord <- list(x = packageMatrix[i,1], y = packageMatrix[i,2])
      }
    }
    if(carInfo$x == nextPackCoord$x) {
      if(carInfo$y == nextPackCoord$y){
        carInfo$nextMove = 5
        return(carInfo)
      }
    }
    frontier = list(list(currPos = list(x = carInfo$x, y = carInfo$y), currCost = 0, pathSearched = list()))
    spaceTheFinalFrontier <- a_star(frontier, trafficMatrix, nextPackCoord, calcHeurMatrix(nextPackCoord, trafficMatrix))
    if(spaceTheFinalFrontier[[1]]$pathSearched[[1]]$y < carInfo$y){
      carInfo$nextMove = 2
      return(carInfo)
    }
    if(spaceTheFinalFrontier[[1]]$pathSearched[[1]]$y > carInfo$y){
      carInfo$nextMove = 8
      return(carInfo)
    }
    if(spaceTheFinalFrontier[[1]]$pathSearched[[1]]$x < carInfo$x){
      carInfo$nextMove = 4
      return(carInfo)
    }
    if(spaceTheFinalFrontier[[1]]$pathSearched[[1]]$x > carInfo$x){
      carInfo$nextMove = 6
      return(carInfo)
    }
  }
  else if (carInfo$load != 0) {
    for(i in 1:nrow(packageMatrix)) {
      if (packageMatrix[i,5] == 1) {
        goalCoord <- list(x = packageMatrix[i,3], y = packageMatrix[i,4])
        if(carInfo$x == goalCoord$x && carInfo$y == goalCoord$y) {
          carInfo$nextMove = 5
          return(carInfo)
        }
        frontier <- list(list(currPos = list(x = carInfo$x, y = carInfo$y), currCost = 0, pathSearched = list()))
        spaceTheFinalFrontier <- a_star(frontier, trafficMatrix, goalCoord, calcHeurMatrix(goalCoord, trafficMatrix))
        if(spaceTheFinalFrontier[[1]]$pathSearched[[1]]$y < carInfo$y){
          carInfo$nextMove = 2
          return(carInfo)
        }
        if(spaceTheFinalFrontier[[1]]$pathSearched[[1]]$y > carInfo$y){
          carInfo$nextMove = 8
          return(carInfo)
        }
        if(spaceTheFinalFrontier[[1]]$pathSearched[[1]]$x < carInfo$x){
          carInfo$nextMove = 4
          return(carInfo)
        }
        if(spaceTheFinalFrontier[[1]]$pathSearched[[1]]$x > carInfo$x){
          carInfo$nextMove = 6
          return(carInfo)
        }
      }
    }
  }
}
a_star <- function(frontier, trafficMatrix, goalCoord, heurMatrix) {
  if (goalCoord$x == frontier[[1]]$currPos$x && goalCoord$y == frontier[[1]]$currPos$y) {
      return(frontier) 
  }
  if(length(frontier[[1]]$pathSearched) > 100){
    return(frontier)
  }
  expPoint = frontier[[1]]$currPos
  prevCost = frontier[[1]]$currCost
  pathSearched = frontier[[1]]$pathSearched
  frontier[[1]] <- NULL
  if(expPoint$x > 1) {
    frontier = expand(expPoint, list(x = expPoint$x - 1, y = expPoint$y), frontier,  heurMatrix, trafficMatrix$hroads[expPoint$x-1, expPoint$y], prevCost, pathSearched)
  }
  if(expPoint$x < 10) {
    frontier = expand(expPoint, list(x = expPoint$x + 1, y = expPoint$y), frontier,  heurMatrix, trafficMatrix$hroads[expPoint$x, expPoint$y], prevCost, pathSearched)
  }
  if(expPoint$y > 1) {
    frontier = expand(expPoint, list(x = expPoint$x, y = expPoint$y - 1), frontier, heurMatrix, trafficMatrix$vroads[expPoint$x, expPoint$y-1], prevCost, pathSearched)
  }
  if(expPoint$y < 10) {
    frontier = expand(expPoint, list(x = expPoint$x, y = expPoint$y + 1), frontier, heurMatrix, trafficMatrix$vroads[expPoint$x, expPoint$y], prevCost, pathSearched)
  }
  return(a_star(frontier, trafficMatrix, goalCoord, heurMatrix))
}
expand <- function(expPoint, addPoint, frontier, heurMatrix, cost, prevCost, pathSearched) {
  if(addPoint$x > 0) {
    if(addPoint$x < 11) {
      if(addPoint$y > 0){
        if(addPoint$y < 11){
          newCost = cost + prevCost
          newFrontierEl = list(currPos = addPoint, currCost = newCost, pathSearched = append(pathSearched, list(addPoint)))
          if(length(frontier) == 0){
            frontier = append(frontier, list(newFrontierEl))
            return(frontier)
          }
          for(i in 1:length(frontier)){
            if(!(frontier[[i]]$currCost + heurMatrix[frontier[[i]]$currPos$x, frontier[[i]]$currPos$y] < newCost + heurMatrix[addPoint$x, addPoint$y])){
              frontier = append(frontier, list(newFrontierEl), after = i-1)
              return(frontier)
            }
            else if(frontier[[i]]$currCost + heurMatrix[frontier[[i]]$currPos$x, frontier[[i]]$currPos$y] == newCost + heurMatrix[addPoint$x, addPoint$y]) {
              frontier = append(frontier, list(newFrontierEl), after = i)
              return(frontier)
            }
            else {
              frontier = append(frontier, list(newFrontierEl))
              return(frontier)
            }
            return(frontier)
          }
          return(frontier)
        }
        return(frontier)
      }
      return(frontier)
    }
    return(frontier)
  }
  return(frontier)
}
calcHeurMatrix <- function(goalCoord, trafficMatrix) {
  heurMat = matrix(nrow = nrow(trafficMatrix$vroads), ncol = ncol(trafficMatrix$hroads))
  for (x in 1:ncol(trafficMatrix$hroads)) {
    for (y in 1:nrow(trafficMatrix$vroads)) {
      heurMat[x,y] = calcMD(x,y,goalCoord[[1]], goalCoord[[2]])
    }
  }
  return(heurMat)
}
calcMD <- function(startPosX, startPosY, goalX, goalY) {
  goalXY <- c(goalX, goalY)
  carXY <- c(startPosX, startPosY)
  dis <- rbind(goalXY,carXY)
  dist <- dist(dis, method= "manhattan")
  manDist = as.matrix(dist)[2,1]
  return(manDist)
}

