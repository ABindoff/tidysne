is.tsner_matrix <- function(x){
  "tsner_matrix" %in% class(x[[1]])
}

is.tsner_fit <- function(x){
  "tsner_fit" %in% class(x)
}

na2zero <- function(x){
  x[is.na(x)] <- 0
  x
}
