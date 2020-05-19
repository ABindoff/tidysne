
select_split <- function(x, ...){
  y <- rlang::enquos(...)
  r1 <- nrow(x)
  x <- tidyr::drop_na(x, !!!y)
  r2 <- nrow(x)
  if(r1 != r2){
    warning(cat("\n", r1, "rows before this operation, but", r1-r2, "rows removed due to missing values in selected columns.\n"))
  }
  a <- x %>% dplyr::select(!!!y) 
  class(a) <- c("tsner_matrix", "data.frame", "tbl", "tbl_df")
  b <- x %>% dplyr::select(-c(!!!y)) 
  class(b) <- c("tsner_grouping", "data.frame", "tbl", "tbl_df")
  list(a, b)
}

# select_split <- function(x, ...){
#   y <- enquos(...)
#   a <- x %>% select(!!!y) 
#   class(a) <- c("tsner_matrix", "data.frame", "tbl", "tbl_df")
#   b <- x %>% select(-c(!!!y)) 
#   class(b) <- c("tsner_grouping", "data.frame", "tbl", "tbl_df")
#   list(a, b)
# }
# 
is.tsner_matrix <- function(x){
  "tsner_matrix" %in% class(x[[1]])
}

is.tsner_fit <- function(x){
  "tsner_fit" %in% class(x)
}


tsner <- function(x, ...){
  if(!is.tsner_matrix(x)){
    stop("\nObject to tsner is not a valid tsner_matrix\nPlease use select_split to produce a tsner_matrix\n")
  }
  m <- Rtsne(x[[1]], ...)
  ys <- data.frame(m$Y)
  names(ys) <- paste0("y", 1:ncol(ys))
  y <- bind_cols(x[[2]], x[[1]], ys)
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
  m <- umap(as.matrix(x[[1]]))
  ys <- data.frame(m$layout)
  names(ys) <- paste0("y", 1:ncol(ys))
  y <- bind_cols(x[[2]], x[[1]], ys)
  attributes(y) <- list(names = names(y),
                        config = m$config,
                        cols = names(x[[1]]))
  class(y) <- c("tsner_fit", "list")
  y
}


clustr <- function(x, ...){
  require(mclust)
  mc <- Mclust(x[[1]])
  x[[2]]$mc <- predict(mc, x[[1]])$classification
  x
}


plotr.foo <- function(g, x, ...){
  y <- as.matrix(x[, g])
  x %>% mutate(att = y,
               taxa = g)
        
  
}


plotr <- function(x, ...){
  if(!is.tsner_fit(x)){
    stop("\nObject to plotr is not a valid tsner_fit\nPlease use tsner to embed\n")
  }
  j <- attributes(x)$cols
  j <- gsub(" ", ".", j)
  k <- lapply(j, function(z) plotr.foo(z, data.frame(x)))
  k <- bind_rows(k)
  class(k) <- c("data.frame", "tbl", "tbl_df", "tsner_plot_layers")
  k
}


order_taxa <- function(x, ...){
  y <- reshape2::melt(x[, c(unique(x$taxa), "y1")], id.vars = c("y1")) %>%
    mutate(z = y1*value) %>%
    group_by(variable) %>%
    summarise(z.m = mean(z, na.rm = TRUE)) %>%
    arrange(z.m)
  x %>% mutate(taxa = factor(taxa, y$variable))
}



superwide <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df <- df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}

na2zero <- function(x){
  x[is.na(x)] <- 0
  x
}


scale_these <- function(x, ..., na.action = NULL){
  cols <- rlang::enquos(...)
  logp0 <- function(y){
    y <- log(y+log_plus)
    y
  }
  if(!is.null(na.action)){
    x <- x %>% mutate_at(cols, na2zero)
  }
  x %>% mutate_at(cols, scale) %>%
    mutate_at(cols, c)
}

foo <- function(x, ...)
{
  rlang::enquos(...)
}

require(Rtsne)
