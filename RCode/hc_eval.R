library(mclust)
library(dplyr)

hc_eval = function(dm) {
  author_labels = sapply(strsplit(colnames(dm), "_"), `[`, 1)
  n = length(unique(author_labels))
  
  hc = hclust(as.dist(dm), method="ward.D")
  hc_n = cutree(hc, k=n)
  
  df = data.frame(FileName = names(hc_n), Cluster = hc_n)
  df$TrueLabel = sapply(strsplit(df$FileName, "_"), `[`, 1)
  
  ari_score = adjustedRandIndex(df$TrueLabel, df$Cluster)
  cl_error =   df %>%
    group_by(Cluster) %>%
    summarise(GroupCount = n_distinct(TrueLabel)) %>%
    summarise(errors = sum(GroupCount - 1)) %>%
    pull(errors) %>%
    as.integer()
  
  return (list("ARI" = ari_score, "Clustering_Errors" = cl_error))
}

# Usage:
# hc_eval(distances_en[["Cosine Delta_lex_1_rel_100"]], n=25)