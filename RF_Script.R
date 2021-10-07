# Tyler Reed

  
rando_forest <- function(x,t,n,d) {
  # two stop statements to check for out of bounds for `n` and `d`
  if (n < 1 | n > nrow(x)) {
    stop("No. of instances chosen not within no. of rows of dataframe.")
  }
  if (d < 1 | d > ncol(x)) {
    stop("No. of attributes chosen not within no. of cols of dataframe.")
  }
  
  # for loop adding `rpart` models to list
  ls <- list()
  for (i in 1:t) {
    str <- "atRisk"
    sep <- "~"
    sam_d <- sample((ncol(x)), d, replace = FALSE)
    # for loop creating string of formula for sampling attributes
    for (j in sam_d) {
      str <- paste0(str, sep, names(x)[j])
      sep <- "+"
    }
    sam_n <- sample(nrow(x), n, replace = TRUE)
    ls[[i]] <- rpart(str, x[sam_n,])
  }
  return (ls)
}

pred <- function(ls, x) {
  # for loop creating dataframe of each models predictions per instance
  df <- cbind(tibble(predict(ls[[1]], x)))
  for (i in 2:length(ls)) {
    df <- cbind(df, data_frame(predict(ls[[i]], x)))
  }
  # for loop creating vector of ensemble random forest predictions by instance
  means <- as_tibble(rowMeans(df))
  vec <- c()
  for (i in 1:nrow(means)) {
    if (means[i, 1] <= 0.50) {
      vec <- rbind(vec, 0)
    } else if (means[i, 1] > 0.50) {
        vec <- rbind(vec, 1)
    }
  }
  return(vec)
}  






