calculate_linear_delta = function(freq_table) {
  # Compute median frequency per word (column-wise median)
  ai = apply(freq_table, 2, median)
  
  # Compute mean absolute deviation from median
  bi = apply(freq_table, 2, function(x) mean(abs(x - ai)))  
  
  # Compute all pairwise absolute differences in vectorized form
  num_docs = nrow(freq_table)
  abs_diffs = abs(outer(1:num_docs, 1:num_docs, Vectorize(function(i, j) sum(abs(freq_table[i, ] - freq_table[j, ]) / bi))))
  
  # self-distance (diag) to zero
  diag(abs_diffs) = 0
  
  return(abs_diffs)
}
