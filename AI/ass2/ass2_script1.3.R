# An R script with a function that can be passed to the runWheresCroc function.
# Needs to....
# Equal or surpass par performance
# Meet the execution time requirement

library("WheresCroc")

# The probability that you get particular sensor readings given Croc is 
# at a particular waterhole is the product of the density value associated 
# with these reading values in the three normal distributions associated with 
# the waterhole. You will need to use the dnorm function to calculate these.
getNormalizedEmissions = function(readings, probs)  {
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
normalize = function(v) {
  sum = sum(v)
  for (i in 1:length(v)) {
    v[i] = v[i] / sum
  }
  return (v)
}

# Returns the probability for Croc transition to any adjacent node (or staying) given being in a specific node.
getTransitionProb = function(edges, node) {
  reachableNodes = getOptions(node, edges)
  return (1/length(reachableNodes))
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
 # print("MOVEINFO")
#  print(moveInfo)
#  print("READINGS")
 # print(readings)
#  print("POSITIONS")
 # print(positions)
#  print("EDGES")
 # print(edges)
#  print("PROBS")
 # print(probs)
  
  transitionProb = getTransitionProb()
  emissionVector = getNormalizedEmissions(readings, probs)
  print("transitionprob")
  print(transitionProb)
  print("emissionvector")
  print(emissionVector)
  
  # Gör forward algorithm för att få fram sannolikheten att Croc är i alla noder
  # Transition probs från de noderna
  
  #spara ner matris i mem så i slipper räkna varje gång
  #nollst
}
