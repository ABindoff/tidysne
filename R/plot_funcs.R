plotr.foo <- function(g, x, ...){
  y <- as.matrix(x[, g])
  x %>% dplyr::mutate(att = y,
               taxa = g)
  
  
}


plotr <- function(x, ...){
  if(!is.tsner_fit(x)){
    stop("\nObject to plotr is not a valid tsner_fit\nPlease use tsner to embed\n")
  }
  j <- attributes(x)$cols
  j <- gsub(" ", ".", j)
  k <- lapply(j, function(z) plotr.foo(z, data.frame(x)))
  k <- dplyr::bind_rows(k)
  class(k) <- c("data.frame", "tbl", "tbl_df", "tsner_plot_layers")
  k
}


order_taxa <- function(x, ...){
  y <- reshape2::melt(x[, c(unique(x$taxa), "y1")], id.vars = c("y1")) %>%
    dplyr::mutate(z = y1*value) %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(z.m = mean(z, na.rm = TRUE)) %>%
    dplyr::arrange(z.m)
  x %>% dplyr::mutate(taxa = factor(taxa, y$variable))
}
