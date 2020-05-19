
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

scale_these <- function(x, ..., na.action = NULL){
  cols <- rlang::enquos(...)
  if(!is.null(na.action)){
    x <- x %>% dplyr::mutate_at(cols, na2zero)
  }
  x %>% dplyr::mutate_at(cols, scale) %>%
    dplyr::mutate_at(cols, c)
}
