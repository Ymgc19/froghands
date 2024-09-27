#' @title 
#' @description \code{fhs_generate_simplex_points}

fhs_convert_2d_to_3d <- function(x, y){
  a <- x - (1/sqrt(3))*y
  b <- y*2/sqrt(3)
  c <- 1 - a - b
  return(c(a, b, c))
}
