# An R script with a function that can be passed to the runWheresCroc function.
# Needs to....
# Equal or surpass par performance
# Meet the execution time requirement

library("WheresCroc")

# The probability that you get particular sensor readings given Croc is 
# at a particular waterhole is the product of the density value associated 
# with these reading values in the three normal distributions associated with 
# the waterhole. You will need to use the dnorm function to calculate these.
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

# Tar en vektor som input och returnerar den normaliserade vektorn
normalize <- function(v) {
  sum = sum(v)
  for (i in 1:length(v)) {
    v[i] = v[i] / sum
  }
  return (v)
}

# Returns the probability for Croc transition to any adjacent node (or staying) given being in a specific node.
getTransitionProb <- function(edges, node) {
  reachableNodes = getOptions(node, edges)
  return (1/length(reachableNodes))
}

# Kollar om en turist dog denna rundan, tar en positions[[1]] som input (alt positions[[2]])
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
    if(i != tourist1 && i != tourist2 && i != ranger){
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
    return(nodeProbs)
  } 
  if(checkDiedTourist(tourist2)) {
    stateProbs[[(-1 * tourist2)]] = 1
    return(nodeProbs)
  } 
  for(i in 1:length(stateProbs)){
    if(i != tourist1 && i != tourist2 && i != ranger) {
      stateProbs[[i]] = 1
      denominator = denominator + 1
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
  # kolla om mem status är 1 eller 0
  # om 0 - räkna ut matris, om 1 - behövs ej pga finns redan
  # status field i mem måste sättas till nått annat för att undvika förvirring?
  # store your current state probabilities in the mem list. Reset this when a new game begins!
  # spara  transition matrix and routing information mellan games
  # 
  # 1.The initial state S0
  # 2.The transition matrix T
  # 3.The emission matrix, E: har att göra med probs. 
  # Sannolikheten att vi observerar nått givet ett visst state. Observationer = sensorvärden
  # Forward hidden: (P(S0=A)P(S1=A|S0=A)+P(S0=B)P(S1=A|S0=B))P(O1=1|S1=A)
  # Kan A och B vara är där/är inte där
  # Vi har redan sannolikheten P(A|B) t.ex. eftersom Croc rör sig randomly uniform.
  # Ta grannarna från nod, 1/(längden av listan+sig själv?) = transition prob
  # getOption(node, edges) för att få info om grannarna
  # Det vi behöver beräkna är P(A) och P(B), detta genom alla värden 
  # vi får från ponds jämfört med sensorvärden.
  # Kolla State Estimation (slide 16): får sannolikhet för state utifrån en sekvens observationer, 
  # normaliserar sen.
  # Forward algorithm slide 17
  
  # Idé: Kolla alla ponds och se vad Croc visar för värden - jämför med alla olika ponds som finns.
  # Kolla sen sannolikheten att Croc befinner sig i varje pond.
  # Måste behålla förra stegets info för att kunna undersöka nästa steg.
  
  
  tourist1 = positions[[1]]
  tourist2 = positions[[2]]
  ranger = positions[[3]]
  
  if(moveInfo$mem$status == 0 || moveInfo$mem$status == 1){
    moveInfo$mem$stateProbs <- initializeProbabilities(tourist1, tourist2, ranger) 
  }
  
  currProbs = moveInfo$mem$stateProbs
  newProbs <- forwardHiddenMarkov(currProbs, probs, readings, edges, tourist1, tourist2, ranger)
  goalNode = which.max(newProbs)
  
  if (goalNode %in% getOptions(ranger, edges)) {
    moveInfo$moves = c(goalNode, 0)
    return(moveInfo)
  }
  
  # Antal noder MELLAN oss och Croc
  pathToGoal = breadthFirstSearch(goalNode, ranger, edges)
  
  # Finns ingen chans att vi kommer stöta på Croc i nästa steg, dvs, ingen mening att söka med 0
  if (length(pathToGoal) > 1) {
    moveInfo$moves = c(pathToGoal[[1]], pathToGoal[[2]])
  }
  # Kan stöta på i nästa steg
  else if (length(pathToGoal) == 1) {
    moveInfo$moves = c(pathToGoal[[1]], 0)
  }
  else {
    moveINfo$moves = c(0,0)
  }
  
  print("path")
  print(pathToGoal)
  
  return(moveInfo)
  
  # Gör forward algorithm för att få fram sannolikheten att Croc är i alla noder
  # Transition probs från de noderna
  
  #spara ner matris i mem så i slipper räkna varje gång
  #nollst
}
