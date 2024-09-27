#' @title convert 3d points to 2d points
#' @description \code{frh_generate_simplex_points}
#' @export

frh_generate_simplex_points <- function(n) {
  # 正三角形の頂点
  P1 <- c(0, 0)
  P2 <- c(1, 0)
  P3 <- c(0.5, sqrt(3)/2)

  # 3つのセクターのブレンドに対応する2次元座標
  points <- data.frame(x = numeric(0), y = numeric(0))
  for (i in 0:n) {
    for (j in 0:(n - i)) {
      lambda1 <- i / n
      lambda2 <- j / n
      lambda3 <- 1 - lambda1 - lambda2
      x <- lambda1 * P1[1] + lambda2 * P2[1] + lambda3 * P3[1]
      y <- lambda1 * P1[2] + lambda2 * P2[2] + lambda3 * P3[2]
      points <- rbind(points, data.frame(x = x, y = y))
    }
  }
  return(points)
}
