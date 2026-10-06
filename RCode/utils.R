#############
# Freq Tables

get_mfw = function(c) {
  lexical_mff = list()
  c_parsed = parse.corpus(c, features = "w", ngram.size = 1, sampling = "no.sampling")
  c_freq_list = make.frequency.list(c_parsed, head = 15000)
  # rel
  c_freq_table = make.table.of.frequencies(c_parsed, c_freq_list, relative = TRUE)
  
  return(c_freq_table)
}

get_mff = function(c, n=1, f="w") {
  c_parsed = parse.corpus(c, features = f, ngram.size = n, sampling = "no.sampling")
  c_freq_list = make.frequency.list(c_parsed, head = 15000)
  # rel
  c_freq_table = make.table.of.frequencies(c_parsed, c_freq_list, relative = TRUE)
  
  return(c_freq_table)
}

######
# Evaluation
######
source("hc_eval.R")
#source("nhc_pam_eval.R")
source("simple_z_score_between_groups.R")

eval_from_distances = function(distances){
  
  n_works = dim(distances[[1]])[1]# differs between refcor (each 75) and ice (66)
  # check on the first dm, it same for the others.
  
  eval = data.frame()
  
  for (key in names(distances)){
    #print(key)#for debugging
    dm = distances[[key]]
    
    primary_acc = 0
    secondary_acc = 0
    either_acc = 0
    both_acc = 0

    for (c in colnames(dm)){
      
      spec_col = dm[, c]
      # from rows remove the c
      spec_col = spec_col[-which(names(spec_col) == c)]
      # sort the column and get the first two
      spec_col = sort(spec_col, decreasing = FALSE)
      
      # author from c. split at _ and take the first part
      base_author = strsplit(c, "_")[[1]][1]
      # first and second author in ordered column in spec_col
      first_author = strsplit(names(spec_col)[1], "_")[[1]][1]
      second_author = strsplit(names(spec_col)[2], "_")[[1]][1]
      
      # primary_acc
      if (base_author == first_author){
        primary_acc = primary_acc + 1
      } 
      
      # secondary_acc
      if (base_author == second_author){
        secondary_acc = secondary_acc + 1
      }
      
      # either_acc
      if (base_author == first_author | base_author == second_author){
        either_acc = either_acc + 1
      }
      
      # both_acc
      if (base_author == first_author & base_author == second_author){
        both_acc = both_acc + 1
      }
    }
    
    # get simple z score diff between in and out groups
    s_score = simple_score(dm) 
    
    # run clustering evaluation methods
    # hoover not symmetric, messes up
    if (startsWith(key, "Hoover")) {
      hoover_symmetric = dm
      hoover_symmetric[lower.tri(hoover_symmetric)] = t(hoover_symmetric)[lower.tri(hoover_symmetric)]
      
      hc_ev <- tryCatch({
        hc_eval(as.matrix(hoover_symmetric))
      }, error = function(e) {
        print("Error for Hoover's P1 Delta (improper values at lower frequencies)[ignore]")
        NULL  # Return NULL on error
      })
      
      if (!is.null(hc_ev)) {
        ari <- hc_ev[["ARI"]]
        cl_err <- hc_ev[["Clustering_Errors"]]
      } else {
        ari <- NA
        cl_err <- NA
      }
      
    } 
    else {
      hc_ev = hc_eval(dm)
      ari = hc_ev[["ARI"]]
      cl_err = hc_ev[["Clustering_Errors"]]
    }
    
    # get the distance, feature and step. Format: distance_feature_step
    result = unlist(strsplit(key, "_"))
    d = result[1]
    f = paste(result[2:4], collapse="_")
    s = result[5]
    # add to dataframe
    #print(c(d, f, s, primary_acc, secondary_acc, either_acc, both_acc))
    cl_correct_pc = (n_works - cl_err) / n_works
    eval = rbind( eval, data.frame(d, f, s, s_score, ari, cl_err, cl_correct_pc, primary_acc, secondary_acc, either_acc, both_acc))
  }
  
  # rename columns
  colnames(eval) = c("distance", "feature", "step", "simple_score", "ari",
                     "cl_error", "cl_correct_pc",
                     "primary_acc", "secondary_acc", "either_acc", "both_acc")
  
  # define correct types for df
  eval = eval %>% 
    mutate(distance = as.factor(distance),
           feature = as.factor(feature),
           step = as.numeric(step),
           simple_score = as.numeric(simple_score),
           ari = as.numeric(ari),
           cl_error = as.numeric(cl_error),
           cl_correct_pc = as.numeric(cl_correct_pc),
           primary_acc = as.numeric(primary_acc),
           secondary_acc = as.numeric(secondary_acc),
           either_acc = as.numeric(either_acc),
           both_acc = as.numeric(both_acc))
  
  # procent
  eval_pc = eval %>% 
    mutate(primary_acc = primary_acc / n_works,
           secondary_acc = secondary_acc / n_works,
           either_acc = either_acc / n_works,
           both_acc = both_acc / n_works)
  
  return (eval_pc)
}
###### Sorting ######
# not trivial
sort_eval_values_at_mff <- function(df) {
  max_performance <- df %>%
    group_by(distance) %>%
    summarise(
      simple_score = max(simple_score),
      cl_error = min(cl_error),
      ari = max(ari),
      .groups = "drop"
    )
  
  max_performance <- max_performance %>%
    mutate(
      rank1 = rank(-simple_score),  # Descending order
      rank2 = rank(cl_error),         # Ascending order
      rank3 = rank(-ari),# Descending order
      mean_rank = rowMeans(across(c(rank1, rank2, rank3))), 
      median_rank = apply(across(c(rank1, rank2, rank3)), 1, median), 
      sum_rank = rowSums(across(c(rank1, rank2, rank3))) 
    ) %>%
    arrange(mean_rank)
  
  highrank <- max_performance$distance
  return(list(max_performance = max_performance, highrank = highrank))
}



######
# Plotting
# most static plotting has been done in python though.
######

library(ggplot2)
library(gridExtra)
library(tidyr)

###### Scatter plots ######

scatter_for_eval <- function(df, n, eval_score, n_dist, max_x, yticks=FALSE, colour="red") {
  df_at_n <- df %>%
    filter(n == !!n) %>%
    arrange(highrank) %>%
    mutate(rank_order = row_number())  # Creating a ranking  order for plotting
  
  plot <- ggplot(df_at_n, aes_string(x = eval_score, y = "rank_order")) +
    geom_point(color = colour) +
    geom_hline(yintercept = 1:n_dist, color = colour, alpha = 0.3) +
    labs(title = paste(n, "MFW"), x = eval_score, y = NULL) +
    theme_minimal()
  
  if (!yticks) {
    plot <- plot + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  } else {
    plot <- plot + scale_y_continuous(breaks = 1:n_dist, labels = df_at_n$delta)
  }
  
  return(plot)
}

plot_results_of_eval <- function(res, num_distances, title=NULL, colour="red") {
  max_x1 <- 4  # From paper or max(res$simple_score + 0.1)
  max_x2 <- 1  # For Adjusted Rand Index
  max_x3 <- 40 # From paper or max(res$cl_err)
  
  p1 <- scatter_for_eval(res, 100, "simple_score", num_distances, max_x1, yticks=TRUE, colour=colour) + xlab("Diff between z-transformed means")
  p2 <- scatter_for_eval(res, 1000, "simple_score", num_distances, max_x1, yticks=TRUE, colour=colour)
  p3 <- scatter_for_eval(res, 5000, "simple_score", num_distances, max_x1, yticks=TRUE, colour=colour)
  
  p4 <- scatter_for_eval(res, 100, "ari", num_distances, max_x2, colour=colour) + xlab("Adjusted Rand Index")
  p5 <- scatter_for_eval(res, 1000, "ari", num_distances, max_x2, colour=colour)
  p6 <- scatter_for_eval(res, 5000, "ari", num_distances, max_x2, colour=colour)
  
  p7 <- scatter_for_eval(res, 100, "cl_error", num_distances, max_x3, colour=colour) + xlab("Cluster Errors")
  p8 <- scatter_for_eval(res, 1000, "cl_error", num_distances, max_x3, colour=colour)
  p9 <- scatter_for_eval(res, 5000, "cl_error", num_distances, max_x3, colour=colour)
  
  grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol=3)
}


######### running eval results #########
plot_eval_results = function(eval, lang = "") {
  # plot primary_acc
  p = ggplot(eval, aes(x = step, y = primary_acc, color = distance)) +
    geom_line() +
    labs(title = paste("Primary Accuracy", lang), x = "Step", y = "Accuracy") +
    theme_minimal()
  
  # plot secondary_acc
  s = ggplot(eval, aes(x = step, y = secondary_acc, color = distance)) +
    geom_line() +
    labs(title = paste("Secondary Accuracy", lang), x = "Step", y = "Accuracy") +
    theme_minimal()
  
  # plot either_acc
  e = ggplot(eval, aes(x = step, y = either_acc, color = distance)) +
    geom_line() +
    labs(title = paste("Either Accuracy", lang), x = "Step", y = "Accuracy") +
    theme_minimal()
  
  # plot both_acc
  b = ggplot(eval, aes(x = step, y = both_acc, color = distance)) +
    geom_line() +
    labs(title = paste("Both Accuracy", lang), x = "Step", y = "Accuracy") +
    theme_minimal()
  
  # Return plots in a list
  return(list(p, s, e, b))
}
######
# as one
plot_eval_results_as_one_test = function(eval, lang = "") {
  # Plot primary_acc
  p = ggplot(eval, aes(x = step, y = primary_acc, color = distance)) +
    geom_line() +
    labs(title = paste("Primary Accuracy", lang), x = "Step", y = "Accuracy") +
    theme_minimal() +
    theme(legend.position = "none")  # Remove legend from this plot
  
  # Plot secondary_acc
  s = ggplot(eval, aes(x = step, y = secondary_acc, color = distance)) +
    geom_line() +
    labs(title = paste("Secondary Accuracy", lang), x = "Step", y = "Accuracy") +
    theme_minimal() +
    theme(legend.position = "none")  # Remove legend from this plot
  
  # Plot either_acc
  e = ggplot(eval, aes(x = step, y = either_acc, color = distance)) +
    geom_line() +
    labs(title = paste("Either Accuracy", lang), x = "Step", y = "Accuracy") +
    theme_minimal() +
    theme(legend.position = "none")  # Remove legend from this plot
  
  # Plot both_acc
  b = ggplot(eval, aes(x = step, y = both_acc, color = distance)) +
    geom_line() +
    labs(title = paste("Both Accuracy", lang), x = "Step", y = "Accuracy") +
    theme_minimal() +
    theme(legend.position = "none")  # Remove legend from this plot
  
  # Get the legend from one of the plots (e.g., the first plot)
  legend <- ggplotGrob(p + theme(legend.position = "bottom"))$grobs[[which(sapply(ggplotGrob(p + theme(legend.position = "bottom"))$grobs, function(x) x$name) == "guide-box")]]
  
  
  # Arrange the plots in a 2x2 grid without legends
  grid_plots <- grid.arrange(p + theme(legend.position = "none"), 
                             s + theme(legend.position = "none"),
                             e + theme(legend.position = "none"),
                             b + theme(legend.position = "none"),
                             ncol = 2, nrow = 2)
  
  # Add the legend at the bottom of the 2x2 plot grid
  grid.arrange(grid_plots, legend, ncol = 2, widths = c(0.8, 0.2))
  
  return(invisible())
}



#######
library(plotly)

plotly_eval_results = function(eval, lang = "") {
  # plot primary_acc
  p = ggplotly(ggplot(eval, aes(x = step, y = primary_acc, color = distance)) +
                 geom_line() +
                 labs(title =paste("Primary Accuracy", lang), x = "Step", y = "Accuracy") +
                 theme_minimal())
  
  # plot secondary_acc
  s = ggplotly(ggplot(eval, aes(x = step, y = secondary_acc, color = distance)) +
                 geom_line() +
                 labs(title = paste("Secondary Accuracy", lang), x = "Step", y = "Accuracy") +
                 theme_minimal())
  
  # plot either_acc
  e = ggplotly(ggplot(eval, aes(x = step, y = either_acc, color = distance)) +
                 geom_line() +
                 labs(title = paste("Either Accuracy", lang), x = "Step", y = "Accuracy") +
                 theme_minimal())
  
  # plot both_acc
  b = ggplotly(ggplot(eval, aes(x = step, y = both_acc, color = distance)) +
                 geom_line() +
                 labs(title = paste("Both Accuracy", lang), x = "Step", y = "Accuracy") +
                 theme_minimal())
  
  simple_score_p = ggplotly(ggplot(eval, aes(x = step, y = simple_score, color = distance)) +
                  geom_line() +
                  labs(title = paste("Difference between z-transformed means", lang), x = "Step", y = "Diff. between z-transformed means") +
                  theme_minimal())
  
  ari_p = ggplotly(ggplot(eval, aes(x = step, y = ari, color = distance)) +
                   geom_line() +
                   labs(title = paste("ARI", lang), x = "Step", y = "ARI") +
                   theme_minimal())
  
  cl_errors_p = ggplotly(ggplot(eval, aes(x = step, y = cl_error, color = distance)) +
                         geom_line() +
                         labs(title = paste("Cluster errors", lang), x = "Step", y = "Cluster errors") +
                         theme_minimal())
  
  # Return plots in a list
  return(list(p, s, e, b, simple_score_p, ari_p, cl_errors_p))
}

library(plotly)

plotly_eval_results_single = function(eval, lang = "") {
  # plot primary_acc
  p = ggplot(eval, aes(x = step, y = primary_acc, color = distance)) +
    geom_line() +
    labs(title =paste("Primary Accuracy", lang), x = "Step", y = "Accuracy") +
    theme_minimal()
  
  # plot secondary_acc
  s = ggplot(eval, aes(x = step, y = secondary_acc, color = distance)) +
    geom_line() +
    labs(title = paste("Secondary Accuracy", lang), x = "Step", y = "Accuracy") +
    theme_minimal()
  
  # plot either_acc
  e = ggplot(eval, aes(x = step, y = either_acc, color = distance)) +
    geom_line() +
    labs(title = paste("Either Accuracy", lang), x = "Step", y = "Accuracy") +
    theme_minimal()
  
  # plot both_acc
  b = ggplot(eval, aes(x = step, y = both_acc, color = distance)) +
    geom_line() +
    labs(title = paste("Both Accuracy", lang), x = "Step", y = "Accuracy") +
    theme_minimal()
  
  # Convert ggplot objects to plotly
  p = ggplotly(p)
  s = ggplotly(s)
  e = ggplotly(e)
  b = ggplotly(b)
  
  subplot(p, s, e, b, nrows = 4, shareX = TRUE, shareY = TRUE) %>%
    layout(title = paste("Accuracy Plots for", lang))
}

plotly_eval_results_one = function(eval, lang = "") {
  # Reshape data to long format for ggplot
  eval_long <- eval %>%
    pivot_longer(cols = c(primary_acc, secondary_acc, either_acc, both_acc, ari, cl_correct_pc, simple_score), 
                 names_to = "Accuracy_Type", 
                 values_to = "Accuracy")
  
  # Create the combined plot
  p <- ggplot(eval_long, aes(x = step, y = Accuracy, color = distance, linetype = Accuracy_Type)) +
    geom_line() +
    labs(title = paste("Accuracy Over Steps", lang), x = "Step", y = "Accuracy", 
         color = "Distance", linetype = "Metric") +
    theme_minimal()
  
  # Convert to plotly for interactivity
  ggplotly(p)
}

plotly_eval_results_one_v2 <- function(eval, lang = "") {
  # Reshape data to long format for ggplot
  eval_long <- eval %>%
    pivot_longer(cols = c(primary_acc, secondary_acc, either_acc, both_acc, ari, cl_correct_pc, simple_score), 
                 names_to = "Accuracy_Type", 
                 values_to = "Accuracy")
  
  # Identify max simple_score for scaling
  max_simple_score <- max(eval$simple_score, na.rm = TRUE)
  
  # Normalize simple_score to fit on a [0,1] scale
  eval_long <- eval_long %>%
    mutate(Scaled_Accuracy = ifelse(Accuracy_Type == "simple_score", Accuracy / max_simple_score, Accuracy))
  
  # Create the plot
  p <- ggplot(eval_long, aes(x = step, y = Scaled_Accuracy, color = distance, linetype = Accuracy_Type)) +
    geom_line() +
    scale_y_continuous(
      name = "Accuracy (0 to 1)", 
      sec.axis = sec_axis(~ . * max_simple_score, name = "Simple Score")
    ) +
    labs(title = paste("Accuracy Over Steps", lang), x = "Step", 
         color = "Distance", linetype = "Metric") +
    theme_minimal()
  
  # Convert to plotly for interactivity
  ggplotly(p)
}


#####Scrap######
plotly_eval_results_for_multiple = function(eval_list) {
  # does not work yet
  # Generate plots for each eval in the list
  plots = lapply(eval_list, plotly_eval_results)
  
  # Arrange the plots in a 3 by 1 grid (assuming there are exactly 3 dataframes in eval_list)
  subplot(plots[[1]], plots[[2]], plots[[3]], nrows = 3, shareX = TRUE, shareY = TRUE)
}

plotly_eval_results_for_multiple = function(eval_list, labels = c("en", "fr", "de")) {
  # Ensure that the number of labels matches the number of eval data frames
  if (length(eval_list) != length(labels)) {
    stop("The number of labels must match the number of evaluation datasets.")
  }
}

plotly_eval_results_for_multiple = function(eval_list) {
  # Generate plots for each eval in the list
  plots = lapply(eval_list, plotly_eval_results)
  
  # Define titles for each plot (e.g., language names)
  titles = c("English", "French", "German")  # Adjust as needed
  
  # Add titles to each plot before combining them
  plots[[1]] <- plots[[1]]
  plots[[2]] <- plots[[2]]
  plots[[3]] <- plots[[3]]
  
  # Create a subplot and arrange the plots in a 3x1 grid
  subplot(plots[[1]], plots[[2]], plots[[3]], 
          nrows = 3, 
          shareX = TRUE, 
          shareY = TRUE)
}