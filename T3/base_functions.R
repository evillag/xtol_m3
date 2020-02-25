library(caret)

fit_model <- function(seed, num_workers=4, ...) {
  #' Parallel multi-threaded wrapper of the caret's train(...) funtion.
  #' @param seed used for the random generator
  #' @param num_workers is the number of worker threads that will be executed in parallel
  #' @param ... parameters passed directly to the caret's train(...) function
  #' @return the trained model
  
  set.seed(seed)
  # Register parallel workers
  cl <- makePSOCKcluster(num_workers)
  registerDoParallel(cl)
  system.time(fitted_model <- train(...))
  # Release workers
  stopCluster(cl)
  return(fitted_model)
}

save_model_to_disk <- function(trained_model, filename="trained_model.rds"){
  #' save the model to disk
  saveRDS(trained_model,  paste("models", filename, sep="/"))
}

plot_correlation <- function(dataset) {
  #' Calculate the correlation among columns in the dataset
  #' and plot a heat diagram with the results
  #' @param dataset Data.frame to analyse
  #' @return correlation data
  corr_data <- cor(dataset)
  
  corrplot(corr_data, type="full", 
           order = "original",
           tl.cex = .85, 
           addCoefasPercent = TRUE,
           col=brewer.pal(n=8, name="RdYlBu"))
  return(corr_data)
}



