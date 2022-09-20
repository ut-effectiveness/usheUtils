dnumber <- function(x, y) {

  if (str_length(x) < 8) {
  x <- str_pad(x, 8, "left", "0")
}

  if (str_starts(x, y) ) {
  x <- str_remove(x, y)

 } else {
    x <- paste0(y, x)
 }
  return(x)
}

dnumber("130751", "D")

