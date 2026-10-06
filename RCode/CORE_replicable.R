library(stylo) #stylometry
library(dplyr) #data crunching

library(arrow) #date export
library(stringr) #regex

source("utils.R") # stylo freq tables, evaluation, and plotting
source("distances.R")# Distances


# load corpora
c_en = load.corpus(corpus.dir = "../corpus/refcor/english/")
c_de = load.corpus(corpus.dir = "../corpus/refcor/german/")
c_fr = load.corpus(corpus.dir = "../corpus/refcor/french/")
#c_ice = skip as you don't have the texts,
#but you have the mfw list
#where features are masked after entry 2000, as F2001,...
#numerical values unchanged though
#see bellow

# mfw = lex 1 grams
en_mfw = get_mfw(c_en)
de_mfw = get_mfw(c_de)
fr_mfw = get_mfw(c_fr)
ice_mfw = readRDS("../Corpus/ice_mfw_masked.rds")

##############
# for lexical 1-grams

### GET DISTANCES
# see distances.R
distances_en = get_distances(en_mfw)
distances_de = get_distances(de_mfw)
distances_fr = get_distances(fr_mfw)
distances_ice = get_distances(ice_mfw)

#### SAVE
save_distances = T

if (save_distances){
  saveRDS(distances_en, file = "../Results/full_distance_matrices/refcor_en_l1g_dist_list_res.rds")
  saveRDS(distances_de, file = "../Results/full_distance_matrices/refcor_de_l1g_dist_list_res.rds")
  saveRDS(distances_fr, file = "../Results/full_distance_matrices/refcor_fr_l1g_dist_list_res.rds")
  saveRDS(distances_ice, file = "../Results/full_distance_matrices/refcor_ice_l1g_dist_list_res.rds")
  }

# export matrices for analysis in Python
export_dm = function(dms, path){
  selected_keys <- names(dms)[str_detect(names(dms), "_(100|1000|5000)$")]
  
  for (name in selected_keys) {
    write_feather(as.data.frame(dms[[name]]), file.path(path, paste0(name, ".feather")))
  }
}

# remove anything in the paths
path = "../Results/selected_distance_matrices/EN/"
file.remove(list.files(path, full.names = TRUE))
path = "../Results/selected_distance_matrices/DE/"
file.remove(list.files(path, full.names = TRUE))
path = "../Results/selected_distance_matrices/FR/"
file.remove(list.files(path, full.names = TRUE))
path = "../Results/selected_distance_matrices/ICE/"
file.remove(list.files(path, full.names = TRUE))

### EXPORT
export_dm(distances_en, "../Results/selected_distance_matrices/EN/")
export_dm(distances_de, "../Results/selected_distance_matrices/DE/")
export_dm(distances_fr, "../Results/selected_distance_matrices/FR/")
export_dm(distances_ice, "../Results/selected_distance_matrices/ICE/")

### Load
### Computation takes long, after computing and working again load the saved matrices###
load = T

if (load) {
  distances_en = readRDS("../Results/full_distance_matrices/refcor_en_l1g_dist_list_res.rds")
  distances_de = readRDS("../Results/full_distance_matrices/refcor_de_l1g_dist_list_res.rds")
  distances_fr = readRDS("../Results/full_distance_matrices/refcor_fr_l1g_dist_list_res.rds")
  distances_ice = readRDS("../Results/full_distance_matrices/refcor_ice_l1g_dist_list_res.rds")
}


###########################################
#EVAL

eval_en = eval_from_distances(distances_en)
eval_de = eval_from_distances(distances_de)
eval_fr = eval_from_distances(distances_fr)
eval_ice = eval_from_distances(distances_ice)

write_feather(eval_en, "../Results/evaluation/refcor_en_l1g_eval_list_res.feather")
write_feather(eval_de, "../Results/evaluation/refcor_de_l1g_eval_list_res.feather")
write_feather(eval_fr, "../Results/evaluation/refcor_fr_l1g_eval_list_res.feather")
write_feather(eval_ice, "../Results/evaluation/refcor_ice_l1g_eval_list_res.feather")

### plot results
plotly_en = plotly_eval_results(eval_en, "(EN RefCor)")
plotly_de = plotly_eval_results(eval_de, "(DE RefCor)")
plotly_fr = plotly_eval_results(eval_fr, "(FR RefCor)")
plotly_ice = plotly_eval_results(eval_ice, "(ICE)")
 