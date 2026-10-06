library(cluster)
library(mclust)
library(dplyr)

pam_eval <- function(dm) {

  author_labels = sapply(strsplit(colnames(dm), "_"), `[`, 1)
  n = length(unique(author_labels))
  
  pam_fit = pam(dm, k = n, diss = TRUE)
  
  df = data.frame(
    FileName = names(pam_fit$clustering),
    Cluster  = pam_fit$clustering
  )
  
  df$TrueLabel = sapply(strsplit(df$FileName, "_"), `[`, 1)
  
  ari_score = adjustedRandIndex(df$TrueLabel, df$Cluster)
  
  cl_error = df %>%
    group_by(Cluster) %>%
    summarise(GroupCount = n_distinct(TrueLabel)) %>%
    summarise(errors = sum(GroupCount - 1)) %>%
    pull(errors) %>%
    as.integer()
  
  return(list(ARI = ari_score, Clustering_Errors = cl_error))
}
