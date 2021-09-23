library("WheresCroc")
getNormalizedEmissions <- function(readings, probs)  {
  salinity = dnorm(readings[1], probs[["salinity"]][, 1], probs[["salinity"]][, 2], FALSE)
  phosphate = dnorm(readings[2], probs[["phosphate"]][, 1], probs[["phosphate"]][, 2], FALSE)
  nitrogen = dnorm(readings[3], probs[["nitrogen"]][, 1], probs[["nitrogen"]][, 2], FALSE)
  emissions = c(1:40)
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
getTransitionProb <- function(edges, node) {
  reachableNodes = getOptions(node, edges)
  return (1/length(reachableNodes))
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
  reachableNodes = getOptions(node, edges)
  stateProb = 0
  for(i in 1:length(reachableNodes)){
    stateProb = stateProb + currProbs[i] * getTransitionProb(edges, i)
  }
  newStateProb = stateProb * emissions[node]
  return(newStateProb)
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
  distances = replicate(40, 0)
  isNodeVisited = rep(FALSE, 40)
  isNodeVisited[ranger] = TRUE
  visitedNodes = replicate(40, NULL)
  while(length(q) > 0) {
    node = q[1]
    q = q[-1]
    reachableNodes = getOptions(node, edges)
    for (reachableNode in reachableNodes) {
      if (reachableNode == goalNode) {
        path = getGoalPath(visitedNodes, ranger, goalNode)
      }
      if (!isNodeVisited[reachableNode]) {
        q = append(q, reachableNode)
        isNodeVisited[reachableNode] = TRUE
        distances[reachableNode] = distances[node] + 1
        visitedNodes[reachableNode] = node
      }
    }
  }
  return (path)
}
getGoalPath <- function(visitedNodes, start, end) {
  path = c()
  at = end
  while(!is.null(at)) {
    path = c(path, visitedNodes[[at]])
    at = visitedNodes[[at]]
  }
  return (rev(path)[2:length(path)])
}
makeMoves <- function(moveInfo, readings, positions, edges, probs) {
  tourist1 = positions[[1]]
  tourist2 = positions[[2]]
  ranger = positions[[3]]
  if(moveInfo$mem$status == 0 || moveInfo$mem$status == 1){
    moveInfo$mem$stateProbs <- initializeProbabilities(tourist1, tourist2, ranger) 
  }
  currProbs = moveInfo$mem$stateProbs
  newProbs <- forwardHiddenMarkov(currProbs, probs, readings, edges, tourist1, tourist2, ranger)
  goalNode = which.max(newProbs)
  if(length(goalNode) == 0){
    moveInfo$moves = c(0,0)
    return(moveInfo)
  }
  for(i in getOptions(ranger,edges)){
    if(i == goalNode){
      moveInfo$moves = c(goalNode, 0)
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
  else {
    moveINfo$moves = c(0,0)
  }
  return(moveInfo)
}
