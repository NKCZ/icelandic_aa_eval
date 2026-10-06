library(stylo)
library(dplyr)
library(ecodist)
library(proxy)

get_distances = function(mfw, steps = c(), f = "lex_1_rel"){
  # set
  #steps = c(100, 1000, 5000)
  if (length(steps)==0){
  steps = c(seq(2, 10-1),
            seq(10, 100-1, 10),
            seq(100, 1000-1, 50), #or 100
            seq(1000, 3000-1, 250), # or 500
            seq(3000, 10000, 500)) # or 1000, it doesn't matter in such high strata
  }
  
  lex_results = list()
  #features = c("lex_1_rel")#c(paste0("lex_", seq(1, 10), "_rel"))
  feature = f
  
  stylo_distances = list(
    "Quadratic Delta" = dist.argamon, #scales; 
    # despite what is claimed in docs, it's not linear, but quadratic  Delta
    "Burrows' Delta" = dist.delta, # scales
    "Cosine" = dist.cosine, # no scales
    "Cosine Delta" = dist.wurzburg, #scales
    "Eder's Simple" = dist.simple, #squares
    "Eder's Delta" = dist.eder, #scales and eders ranking
    "Entropy" = dist.entropy,
    "Min-Max" = dist.minmax
  )
  
  extra_distances = list(
    "Euclidian" = "euclidian",
    "Manhattan" = "manhattan",
    "Canberra" = "canberra",
    "Chebyshev" = "maximum"
  )
  
  #stylo dists
  for (distance in names(stylo_distances)){
    print(distance)
    for (i in steps){
      key = paste0(distance, "_", feature, "_", i)
      dm = stylo_distances[[distance]](mfw[, 1:i])
      dm = as.matrix(dm)
      lex_results[[key]] = dm
    }
  }
  
  # extra dists
  for (distance in names(extra_distances)){
    print(distance)
    for (i in steps){
      key = paste0(distance, "_", feature, "_", i)
      dm = as.matrix(stats::dist(mfw[, 1:i],
                                 method = extra_distances[[distance]]))
      lex_results[[key]] = dm
    }
  }
  
  #### LINEAR DELTA
  source("LinearDelta.R")
  dist_str = "Linear Delta"
  print(dist_str)
  
  for (i in steps){
    #print(i) #for debugging, it works, but slow
    key = paste0(dist_str, "_", feature, "_", i)
    fr_t = mfw[, 1:i]
    dm = as.matrix(calculate_linear_delta(fr_t))
    rownames(dm) = colnames(dm) = rownames(fr_t)
    lex_results[[key]] = dm
  }
  #### Hoover P1
  source("hooverp1.R")
  dist_str = "Hoover's P1 Delta"
  print(dist_str)
  
  for (i in steps){
    key = paste0(dist_str, "_", feature, "_", i)
    fr_t = mfw[, 1:i]
    dm = hoover_p1(fr_t)
    lex_results[[key]] = dm
  }
  
  # BRAY CURTIS
  distance_str = "Bray-Curtis"
  print(distance_str)
  # Bray-Curtis
  for (i in steps){
    key = paste0(distance_str, "_", feature, "_", i)
    dm = as.matrix(ecodist::bcdist(mfw[, 1:i]))
    lex_results[[key]] = dm
  }
  
  # CORR
  
  distance_str = "Correlation"
  print(distance_str)
  for (i in steps){
    key = paste0(distance_str, "_", feature, "_", i)
    dm = as.matrix(proxy::dist((mfw[, 1:i]), method = function(a, b) 1 - cor(a, b)))
    lex_results[[key]] = dm
  }
  return (lex_results) 
}
