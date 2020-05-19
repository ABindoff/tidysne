tsner <- function(x, ...){
  if(!is.tsner_matrix(x)){
    stop("\nObject to tsner is not a valid tsner_matrix\nPlease use select_split to produce a tsner_matrix\n")
  }
  m <- Rtsne::Rtsne(x[[1]], ...)
  ys <- data.frame(m$Y)
  names(ys) <- paste0("y", 1:ncol(ys))
  y <- dplyr::bind_cols(x[[2]], x[[1]], ys)
  attributes(y) <- list(names = names(y),
                        perplexity = m$perplexity,
                        N = m$N,
                        min_kl = m$itercosts[length(m$itercosts)],
                        max_iter = m$max_iter,
                        theta = m$theta,
                        origD = m$origD,
                        eta = m$eta,
                        cols = names(x[[1]]))
  class(y) <- c("tsner_fit", "list")
  y
}


umapr <- function(x, ...){
  if(!is.tsner_matrix(x)){
    stop("\nObject to tsner is not a valid tsner_matrix\nPlease use select_split to produce a tsner_matrix\n")
  }
  m <- umap::umap(as.matrix(x[[1]]))
  ys <- data.frame(m$layout)
  names(ys) <- paste0("y", 1:ncol(ys))
  y <- bind_cols(x[[2]], x[[1]], ys)
  attributes(y) <- list(names = names(y),
                        config = m$config,
                        cols = names(x[[1]]))
  class(y) <- c("tsner_fit", "list")
  y
}


# clustr <- function(x, ...){
#   require(mclust)
#   mc <- mclust::Mclust(x[[1]])
#   x[[2]]$mc <- stats::predict(mc, x[[1]])$classification
#   x
# }
