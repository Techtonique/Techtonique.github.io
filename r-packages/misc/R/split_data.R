#' Split a dataset
#' 
#' @param y A vector of labels
#' @param p A proportion of the dataset to split
#' @param seed An integer to set the seed
#' @param type_split A character string specifying the type of split
#' @return A vector of indices
#' @export
#' @examples
#' 
#' set.seed(123)
#' (y <- rnorm(10))
#' misc::split_data(y, 0.5)
#' misc::split_data(y, 0.5, type_split = "sequential")
#' 
split_data <- function(y, p = 0.5, 
                       seed = 123,
                       type_split=c("stratify", 
                                    "sequential")) {
  
  type_split <- match.arg(type_split)
  
  if (identical(type_split, "sequential"))
  {
    return(seq_len(length(y)*p))
  }
  
  # from caret::createFolds
  set.seed(seed)
  stopifnot((p > 0) && (p < 1))
  k <- floor(1 / p)
  return_list <- TRUE
  returnTrain <- FALSE
  
  if (inherits(y, "Surv"))
    y <- y[, "time"]
  if (is.numeric(y)) {
    cuts <- floor(length(y) / k)
    if (cuts < 2)
      cuts <- 2
    if (cuts > 5)
      cuts <- 5
    breaks <-
      unique(stats::quantile(y, probs = seq(0, 1, length = cuts)))
    y <- cut(y, breaks, include.lowest = TRUE)
  }
  if (k < length(y)) {
    y <- factor(as.character(y))
    numInClass <- table(y)
    foldVector <- vector(mode = "integer", length(y))
    for (i in 1:length(numInClass)) {
      min_reps <- numInClass[i] %/% k
      if (min_reps > 0) {
        spares <- numInClass[i] %% k
        seqVector <- rep(1:k, min_reps)
        if (spares > 0)
          seqVector <- c(seqVector, sample(1:k, spares))
        foldVector[which(y == names(numInClass)[i])] <-
          sample(seqVector)
      } else {
        foldVector[which(y == names(numInClass)[i])] <-
          sample(1:k, size = numInClass[i])
      }
    }
  } else {
    foldVector <- seq(along = y)
  }
  
  out <- split(seq(along = y), foldVector)
  
  return(out[[1]])
}
