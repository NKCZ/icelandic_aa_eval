library(stylo) #stylometry
library(dplyr) #data crunching

library(arrow) #date export
library(stringr) #regex

source("utils.R") # stylo freq tables, evaluation, and plotting
source("distances.R")

#load corpus
c_ice = load.corpus(corpus.dir = "E:/Users/N/DH/MasterThesis/ocr/stylo_ice")

# mfw = lex 1 grams
ice_l1g = get_mfw(c_ice)
##############
# for lexical 1-grams

### GET DISTANCES
# see distances.R
distances_ice = get_distances(ice_mfw)

#### SAVE

#saveRDS(distances_ice, "")
# export matrices for analysis in Python

export_dm = function(dms, path){
  selected_keys <- names(dms)[str_detect(names(dms), "_(100|1000|5000)$")]
  
  for (name in selected_keys) {
    write_feather(as.data.frame(dms[[name]]), file.path(path, paste0(name, ".feather")))
  }
}

# remove anything in the paths
#path = "./refcor_matrices/EN/"
#file.remove(list.files(path, full.names = TRUE))
#export_dm(distances_en, "./refcor_matrices/EN/")

### Load
load = F

if (load) {
  #distances_en = readRDS("E:/Users/N/DH/MasterThesis/Styl_Eval/refcor_results/refcor_en_l1g_dist_list_res.rds")
}


###########################################
#EVAL

eval_ice = eval_from_distances(distances_ice, n_works = 66)

#write_feather(eval_en, "./refcor_results/refcor_en_l1g_eval_list_res.feather")


### plot results
plotly_ice = plotly_eval_results(eval_ice, "(ICE)")



#
# Group by distance and calculate correlation matrix for numeric columns
correlations_by_distance <- eval_ice %>%
  group_by(distance) %>%
  select(where(is.numeric)) %>%
  summarise(
    cor_matrix = list(cor(select(.)))
  )

# Print the result
print(correlations_by_distance)