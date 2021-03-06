library("DeliveryMan")
myFunction <- function(trafficMatrix, carInfo, packageMatrix) {
  mdToNextPack = 100
  if(carInfo$load == 0) {
    for(i in 1:nrow(packageMatrix)) {
      distanceToPackage <- calcMD(carInfo$x, carInfo$y, packageMatrix[i,1], packageMatrix[i,2])
      if (distanceToPackage < mdToNextPack && packageMatrix[i,5] == 0) {
        mdToNextPack = distanceToPackage
        nextPackCoord <- list(x = packageMatrix[i,1], y = packageMatrix[i,2])
      }
    }
    if(carInfo$x == nextPackCoord$x & carInfo$y == nextPackCoord$y) {
        carInfo$nextMove = 5
        return(carInfo)
    }
    frontier = list(list(currPos = list(x = carInfo$x, y = carInfo$y), currCost = 0, pathSearched = list()))
    spaceTheFinalFrontier <- a_star(frontier, trafficMatrix, nextPackCoord, calcHeurMatrix(nextPackCoord, trafficMatrix))
    if(spaceTheFinalFrontier[[1]]$pathSearched[[1]]$y < carInfo$y){
      carInfo$nextMove = 2
    }
    if(spaceTheFinalFrontier[[1]]$pathSearched[[1]]$y > carInfo$y){
      carInfo$nextMove = 8
    }
    if(spaceTheFinalFrontier[[1]]$pathSearched[[1]]$x < carInfo$x){
      carInfo$nextMove = 4
    }
    if(spaceTheFinalFrontier[[1]]$pathSearched[[1]]$x > carInfo$x){
      carInfo$nextMove = 6
    }
    if(spaceTheFinalFrontier[[1]]$pathSearched[[1]]$x == carInfo$x && spaceTheFinalFrontier[[1]]$pathSearched[[1]]$y == carInfo$y){
      carInfo$nextMove = 5
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
        }
        if(spaceTheFinalFrontier[[1]]$pathSearched[[1]]$y > carInfo$y){
          carInfo$nextMove = 8
        }
        if(spaceTheFinalFrontier[[1]]$pathSearched[[1]]$x < carInfo$x){
          carInfo$nextMove = 4
        }
        if(spaceTheFinalFrontier[[1]]$pathSearched[[1]]$x > carInfo$x){
          carInfo$nextMove = 6
        }
      }
    }
  }
  return(carInfo)
}

a_star <- function(frontier, trafficMatrix, goalCoord, heurMatrix) {
  if (goalCoord$x == frontier[[1]]$currPos$x && goalCoord$y == frontier[[1]]$currPos$y) {
    return(frontier)
  }
  expPoint = frontier[[1]]$currPos
  prevCost = frontier[[1]]$currCost
  pathSearched = frontier[[1]]$pathSearched
  frontier[[1]] <- NULL
  frontier = expand(expPoint, list(x = expPoint$x - 1, y = expPoint$y), frontier,  heurMatrix, trafficMatrix$hroads[expPoint$x-1, expPoint$y], prevCost, pathSearched)
  frontier = expand(expPoint, list(x = expPoint$x + 1, y = expPoint$y), frontier,  heurMatrix, trafficMatrix$hroads[expPoint$x, expPoint$y], prevCost, pathSearched)
  frontier = expand(expPoint, list(x = expPoint$x, y = expPoint$y - 1), frontier, heurMatrix, trafficMatrix$vroads[expPoint$x, expPoint$y-1], prevCost, pathSearched)
  frontier = expand(expPoint, list(x = expPoint$x, y = expPoint$y + 1), frontier, heurMatrix, trafficMatrix$vroads[expPoint$x, expPoint$y], prevCost, pathSearched)
  return(a_star(frontier, trafficMatrix, goalCoord, heurMatrix))
}

expand <- function(expPoint, addPoint, frontier, heurMatrix, cost, prevCost, pathSearched) {
  if(addPoint$x > 0 & addPoint$x < 11) {
    if(addPoint$y > 0 & addPoint$y < 11){
      newCost = cost + prevCost
      newFrontierEl = list(currPos = addPoint, currCost = newCost, pathSearched = append(pathSearched, list(addPoint)))
      if(length(frontier) == 0){
        frontier[[1]] = newFrontierEl
      }
      else if(frontier[[length(frontier)]]$currCost + heurMatrix[frontier[[length(frontier)]]$currPos$x, frontier[[length(frontier)]]$currPos$y] < newCost + heurMatrix[addPoint$x, addPoint$y]){
        frontier[[length(frontier)+1]] = newFrontierEl
      }
      else {
        for(i in 1:length(frontier)) {
          if(newCost + heurMatrix[addPoint$x, addPoint$y] <= frontier[[i]]$currCost + heurMatrix[frontier[[i]]$currPos$x, frontier[[i]]$currPos$y]) {
            frontier = append(frontier, list(newFrontierEl), after = i-1)
            break
          }
        }
      }
      samePoint = FALSE
      for(i in 1:length(frontier)){
        if(frontier[[i]]$currPos$x == addPoint$x & frontier[[i]]$currPos$y == addPoint$y){
          if(!samePoint){
            samePoint = TRUE
          }
          else {
            frontier[[i]] <- NULL
            break
          }
        }
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

