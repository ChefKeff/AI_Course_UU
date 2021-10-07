library("WheresCroc")
# PASSED!
getNormalizedEmissions <- function(readings, probs)  {
  salinity = dnorm(readings[1], probs[["salinity"]][, 1], probs[["salinity"]][, 2], FALSE)
  phosphate = dnorm(readings[2], probs[["phosphate"]][, 1], probs[["phosphate"]][, 2], FALSE)
  nitrogen = dnorm(readings[3], probs[["nitrogen"]][, 1], probs[["nitrogen"]][, 2], FALSE)
  emissions = replicate(40,0)
  for (i in 1:40) {
    emissions[i] = salinity[i] * phosphate[i] * nitrogen[i]
  }
  normalizedEmissions = normalize(emissions)
  return (normalizedEmissions)
}
normalize <- function(v) {
  sum = sum(v)
  for (i in 1:length(v)) {
    v[i] = v[i] / sum
  }
  return (v)
}
getReachableNodes <- function(point,edges) {
  c(edges[which(edges[,1] == point), 2], edges[which(edges[,2] == point), 1], point)
}
getTransitionProb <- function(edges, node) {
  reachableNodes = getReachableNodes(node, edges)
  return(1/length(reachableNodes))
}
checkDiedTourist <- function(tourist) {
  if(!is.na(tourist) && tourist < 0) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}
getStateProb <- function(currProbs, node, edges, emissions){
  reachableNodes = getReachableNodes(node, edges)
  stateProb = 0
  for(rNode in reachableNodes){
    stateProb = stateProb + currProbs[rNode] * getTransitionProb(edges, rNode) * emissions[node]
  }
  return(stateProb)
}
forwardHiddenMarkov <- function(currProbs, probs, readings, edges, tourist1, tourist2, ranger){
  stateProbs = replicate(40, 0)
  if(checkDiedTourist(tourist1)) {
    stateProbs[[(-1 * tourist1)]] = 1
    return(normalize(stateProbs))
  } 
  if(checkDiedTourist(tourist2)) {
    stateProbs[[(-1 * tourist2)]] = 1
    return(normalize(stateProbs))
  } 
  emissions = getNormalizedEmissions(readings, probs)
  for(i in 1:length(stateProbs)){
    if(!is.na(tourist1) && !is.na(tourist2)){
      if(i != tourist1 && i != tourist2 && i != ranger){
        stateProbs[i] = getStateProb(currProbs, i, edges, emissions)
      }
    } else if(is.na(tourist1) && !is.na(tourist2)){
      if(i != tourist2 && i != ranger){}
      stateProbs[i] = getStateProb(currProbs, i, edges, emissions)
    } else if(!is.na(tourist1) && is.na(tourist2)){
      if(i != tourist1 && i != ranger){}
      stateProbs[i] = getStateProb(currProbs, i, edges, emissions)
    } else {
      if(i != ranger){}
      stateProbs[i] = getStateProb(currProbs, i, edges, emissions)
    }
  }
  return(normalize(stateProbs))
}
initializeProbabilities <- function(tourist1, tourist2, ranger){
  stateProbs = replicate(40, 0)
  denominator = 0
  if(checkDiedTourist(tourist1)) {
    stateProbs[[(-1 * tourist1)]] = 1
    return(stateProbs)
  } 
  if(checkDiedTourist(tourist2)) {
    stateProbs[[(-1 * tourist2)]] = 1
    return(stateProbs)
  } 
  for(i in 1:length(stateProbs)){
    if(!is.na(tourist1) && !is.na(tourist2)){
      if(i != tourist1 && i != tourist2 && i != ranger) {
        stateProbs[[i]] = 1
        denominator = denominator + 1
      }
    }
  }
  stateProbs = stateProbs / denominator
  return(stateProbs)
}
breadthFirstSearch <- function(goalNode, ranger, edges) {
  q = c(ranger)
  isNodeVisited = rep(FALSE, 40)
  isNodeVisited[ranger] = TRUE
  parents = replicate(40, 0)
  parents[[ranger]] = -1
  while(length(q) > 0) {
    node = q[1]
    q = q[-1]
    reachableNodes = getReachableNodes(node, edges)
    for (reachableNode in reachableNodes) {
      if (!isNodeVisited[reachableNode]) {
        q = append(q, reachableNode)
        isNodeVisited[reachableNode] = TRUE
        parents[reachableNode] = node
      }
    }
  }
  at = goalNode
  path = c()
  while (at != -1) {
    if (parents[at] != -1) {
      path = c(c(at), path)
    }
    at = parents[at]
  }
  return (path)
}
myFunction <- function(moveInfo, readings, positions, edges, probs) {
  tourist1 = positions[[1]]
  tourist2 = positions[[2]]
  ranger = positions[[3]]
  if(moveInfo$mem$status == 0 || moveInfo$mem$status == 1){
    moveInfo$mem$stateProbs <- initializeProbabilities(tourist1, tourist2, ranger) 
  }
  currProbs = moveInfo$mem$stateProbs
  newProbs <- forwardHiddenMarkov(currProbs, probs, readings, edges, tourist1, tourist2, ranger)
  goalNode <- which.max(newProbs)
  for(i in getReachableNodes(ranger, edges)){
    if(i == goalNode){
      moveInfo$moves = c(goalNode, 0)
      moveInfo$mem$stateProbs = newProbs
      return(moveInfo)
    }
  }
  pathToGoal = breadthFirstSearch(goalNode, ranger, edges)
  if (length(pathToGoal) > 1) {
    moveInfo$moves = c(pathToGoal[[1]], pathToGoal[[2]])
  }
  else if (length(pathToGoal) == 1) {
    moveInfo$moves = c(pathToGoal[[1]], 0)
  }
  moveInfo$mem$stateProbs = newProbs
  moveInfo$mem$status = 2
  return(moveInfo)
}
