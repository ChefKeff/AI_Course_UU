learn <- function(historical_data) {
  pPn = sum(historical_data$Pn) / length(historical_data$Pn)
  pSm = sum(historical_data$Sm) / length(historical_data$Sm)
  pVTB = sum(historical_data$VTB) / length(historical_data$VTB)
  print('ppn')
  print(pPn)
  print('psm')
  print(pSm)
  print('pvtb')
  print(pVTB)
}

# VARIABLES IN HISTORICAL DATA
# Pn = pneumonia
# Te = temperature
# VTB = Visited TB spot (?)
# TB = Tuberculosis
# Sm = Smoking
# LC = Lung cancer
# Br = Bronchitis
# XR = X-ray results
# Dy = Dyspnea

diagnose <- function(network, unknown) {
  print(network)
  print(unknown)
}
