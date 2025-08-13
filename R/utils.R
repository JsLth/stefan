random_name <- function(size = 10) {
  paste(sample(c(LETTERS, letters), size = size, replace = TRUE), collapse = "")
}
