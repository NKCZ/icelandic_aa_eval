# implentation of hoover p1 and pielström p1 from PyDelta

# this one runs fast. smb. should double check if correct though
hoover_p1 = function(corpus) {
  # Z-score normalization
  z_scores = scale(corpus, center = TRUE, scale = TRUE)
  
  rows = rownames(corpus)
  n = length(rows)
  
  deltas = matrix(0, ncol = n, nrow = n, dimnames = list(rows, rows))
  
  row_indices = 1:n
  
  # vectorized delta computation 
  compute_delta_vec = function(i, j) {
    if (identical(i, j)) return(0)
    
    zdiff = z_scores[i, ] - z_scores[j, ]
    zdpos = zdiff[zdiff >= 0]
    zdneg = zdiff[zdiff < 0]
    
    return((mean(zdpos, na.rm = TRUE) + 1)^2 - mean(zdneg, na.rm = TRUE))
  }
  
  # outer() for vectorised
  deltas = outer(row_indices, row_indices, Vectorize(compute_delta_vec))
  rownames(deltas) = rows
  colnames(deltas) = rows
  
  return(deltas)
}

# ignore, naive approaches
# hoover_p1 = function(corpus) {
#   z_scores = as.data.frame(scale(corpus, center = TRUE, scale = TRUE))  # Column-wise Z-score normalization
#   
#   rows = rownames(corpus)
#   row_combinations = expand.grid(a = rows, b = rows, KEEP.OUT.ATTRS = FALSE)  # Get all row combinations
#   
#   # Function to compute delta values
#   compute_delta = function(a, b) {
#     if (a == b) return(0)  # Skip self-comparison
#     
#     zdiff = as.numeric(z_scores[a, ] - z_scores[b, ])
#     zdpos = zdiff[zdiff >= 0]
#     zdneg = zdiff[zdiff < 0]
#     
#     return((mean(zdpos, na.rm = TRUE) + 1)^2 - mean(zdneg, na.rm = TRUE))
#   }
#   
#   # Apply function to all row pairs
#   deltas = matrix(0, ncol = length(rows), nrow = length(rows), 
#                    dimnames = list(rows, rows))
#   
#   deltas_values = apply(row_combinations, 1, function(r) compute_delta(r[1], r[2]))
#   #delta_values = outer(1:num_docs, 1:num_docs, Vectorize(function(i, j) compute_delta(corpus[i, ], corpus[j, ])))     
#   
#   deltas[as.matrix(row_combinations)] = deltas_values  # Assign computed values to matrix
#   
#   return(deltas)
# }


#########################
# hoover_p1 = function(corpus) {
#   z_scores = as.data.frame(scale(corpus, center = TRUE, scale = TRUE))  # Column-wise Z-score normalization
#   
#   deltas = matrix(0, ncol = nrow(corpus), nrow = nrow(corpus), 
#                    dimnames = list(rownames(corpus), rownames(corpus)))
#   
#   rows = rownames(corpus)
#   
#   for (a in rows) {
#     for (b in rows) {
#       if (a != b) {
#         zdiff = as.numeric(z_scores[a, ] - z_scores[b, ])
#         zdpos = zdiff[zdiff >= 0]
#         zdneg = zdiff[zdiff < 0]
#         
#         deltas[a, b] = (mean(zdpos, na.rm = TRUE) + 1)^2 - mean(zdneg, na.rm = TRUE)
#       }
#     }
#   }
#   
#   return(deltas)#as.data.frame(deltas))
# }
# 
# hoover_p1 = function(corpus) {
#   # Column-wise Z-score normalization
#   z_scores = as.data.frame(scale(corpus, center = TRUE, scale = TRUE))  
#   
#   # Preallocate the matrix for efficiency
#   deltas = matrix(NA, ncol = nrow(corpus), nrow = nrow(corpus),
#                    dimnames = list(rownames(corpus), rownames(corpus)))
#   
#   rows = rownames(corpus)
#   
#   # Compute pairwise differences using apply
#   deltas = sapply(rows, function(a) {
#     sapply(rows, function(b) {
#       if (a == b) return(NA)  # Skip self-comparisons
#       
#       diff = as.numeric(z_scores[a, ] - z_scores[b, ])
#       
#       zdpos = diff[diff >= 0]
#       zdneg = diff[diff < 0]
#       
#       (mean(zdpos, na.rm = TRUE) + 1)^2 - mean(zdneg, na.rm = TRUE)
#     })
#   })
#   
#   # Ensure deltas is a matrix with correct row and column names
#   dimnames(deltas) = list(rows, rows)
#   
#   diag(deltas) = 0
#   
#   return(deltas)
# }


# # better?
# hoover_p1 = function(corpus) {
#   # Perform column-wise Z-score normalization
#   z_scores = scale(corpus, center = TRUE, scale = TRUE)
#   
#   # Calculate pairwise differences between all rows (no loops)
#   zdiff_matrix = apply(z_scores, 1, function(x) x - z_scores)
#   
#   # Calculate the positive and negative differences
#   zdpos_matrix = pmax(zdiff_matrix, 0)  # positive differences
#   zdneg_matrix = pmin(zdiff_matrix, 0)  # negative differences
#   
#   # Compute the deltas for all pairs
#   mean_zdpos = rowMeans(zdpos_matrix, na.rm = TRUE)  # mean positive diffs
#   mean_zdneg = rowMeans(zdneg_matrix, na.rm = TRUE)  # mean negative diffs
#   
#   # Calculate deltas for each pair without loops
#   deltas = (mean_zdpos + 1)^2 - mean_zdneg
#   deltas = matrix(deltas, nrow = nrow(corpus), ncol = nrow(corpus), byrow = TRUE,
#                   dimnames = list(rownames(corpus), rownames(corpus)))
#   # Return the result as a matrix
#   return(deltas)
# }

# symmetric
pielstroem_p1 = function(corpus) {
  z_scores = as.data.frame(scale(corpus, center = TRUE, scale = TRUE))
  
  deltas = matrix(0, ncol = nrow(corpus), nrow = nrow(corpus), 
                   dimnames = list(rownames(corpus), rownames(corpus)))
  
  row_pairs = combn(rownames(corpus), 2, simplify = FALSE)
  
  for (pair in row_pairs) {
    a = pair[1]
    b = pair[2]
    
    zdiff = as.numeric(z_scores[a, ] - z_scores[b, ])
    zdpos = zdiff[zdiff >= 0]
    zdneg = zdiff[zdiff < 0]
    
    if (sum(zdpos, na.rm = TRUE) < -sum(zdneg, na.rm = TRUE)) {
      temp = zdpos
      zdpos = -zdneg
      zdneg = -temp
    }
    
    d = (mean(zdpos, na.rm = TRUE) + 1)^2 - mean(zdneg, na.rm = TRUE)
    
    deltas[a, b] = d
    deltas[b, a] = d
  }
  
  return(as.data.frame(deltas))
}
