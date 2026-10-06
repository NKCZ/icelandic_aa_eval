# Simple Score as in PyDelta and Jannidis et al. 2015

simple_score = function(distance_matrix) {
  
  author_labels = sapply(strsplit(colnames(distance_matrix), "_"), `[`, 1)
  
  # Get upper triangle values
  upper_triangle_mask = upper.tri(distance_matrix)
  upper_triangle_values = distance_matrix[upper_triangle_mask]
  
  # Standardize the matrix
  dm_mean = mean(upper_triangle_values)
  dm_std = sd(upper_triangle_values)
  standardised_matrix = (distance_matrix - dm_mean) / dm_std
  standardised_matrix[!upper_triangle_mask] = 0  # fill with 0 the lower triangle
  
  # In-group and out-group 
  is_in_group = outer(author_labels, author_labels, FUN = "==")
  
  in_group_values = standardised_matrix[upper_triangle_mask & is_in_group]
  out_group_values = standardised_matrix[upper_triangle_mask & !is_in_group]
  
  difference = mean(out_group_values, na.rm = TRUE) - mean(in_group_values, na.rm = TRUE)
  return(difference)
}


# Usage
#distance_matrix = distances_en[["Cosine Delta_lex_1_rel_1000"]]
#simple_score(distance_matrix)


# simple_score = function(distance_matrix) {
#   
#   author_labels = sapply(strsplit(colnames(distance_matrix), "_"), `[`, 1)
#   
#   upper_triangle_mask = upper.tri(distance_matrix)
#   upper_triangle_matrix = distance_matrix*upper_triangle_mask
#   upper_triangle_vector = as.vector(upper_triangle_matrix)
#   upper_triangle_vector = upper_triangle_vector[upper_triangle_vector != 0]
#   
#   dm_mean = mean(upper_triangle_vector)
#   dm_std = sd(upper_triangle_vector)
#   standardised_matrix = (distance_matrix - dm_mean) / dm_std
#   
#   upper_triangle_mask = upper.tri(standardised_matrix)
#   standardised_matrix = standardised_matrix*upper_triangle_mask
#   
#   # Calculate in-group and out-group
#   is_in_group = outer(author_labels, author_labels, FUN = "==")
#   
#   in_group = standardised_matrix * is_in_group
#   out_group = standardised_matrix * !is_in_group
#   
#   in_group = as.vector(in_group)
#   in_group = in_group[in_group != 0]
#   out_group = as.vector(out_group)
#   out_group = out_group[out_group !=0]
#   # Compute the mean of in-group and out-group
#   mean_in_group = mean(in_group, na.rm = TRUE)
#   mean_out_group = mean(out_group, na.rm = TRUE)
#   
#   difference = mean_out_group - mean_in_group
#   return(difference)
# }

# Usage:
#dm = distances_en[["Cosine Delta_lex_1_rel_100"]]
#distance_matrix = distances_en[["Cosine Delta_lex_1_rel_1000"]]
#simple_score(distance_matrix)


##### 
# Implementation from PyDelta. It can be done simpler, see above
# z_scores = function(distance_matrix) {
#   standardised_matrix = scale(distance_matrix)
#   return(standardised_matrix)
# }
# 
# # Helper function to partition the matrix into in-group and out-group
# partition = function(distance_matrix, author_labels) {
#   n = nrow(distance_matrix)
#   in_group = matrix(NA, nrow = n, ncol = n)
#   out_group = matrix(NA, nrow = n, ncol = n)
#   
#   for (i in 1:n) {
#     for (j in 1:n) {
#       if (author_labels[i] == author_labels[j]) {
#         in_group[i, j] = distance_matrix[i, j]
#       } else {
#         out_group[i, j] = distance_matrix[i, j]
#       }
#     }
#   }
#   
#   return(list(in_group = in_group, out_group = out_group))
# }
# 
# # Simple score calculation function
# simple_score = function(distance_matrix, author_labels) {
#   # Standardize the distance matrix
#   standardised_matrix = z_scores(distance_matrix)
#   
#   # Partition the matrix into in-group and out-group
#   partitions = partition(standardised_matrix, author_labels)
#   
#   # Flatten in-group and out-group matrices to calculate means
#   in_group = as.vector(partitions$in_group)
#   out_group = as.vector(partitions$out_group)
#   
#   # Remove NA values which are present for non-relevant comparisons
#   in_group = in_group[!is.na(in_group)]
#   out_group = out_group[!is.na(out_group)]
#   
#   # Calculate the means of in-group and out-group
#   mean_in_group = mean(in_group)
#   mean_out_group = mean(out_group)
#   
#   # Return the simple score (difference between means)
#   score = mean_out_group - mean_in_group
#   return(score)
# }
