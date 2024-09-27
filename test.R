devtools::install_github("Ymgc19/froghands", force = TRUE)

# 効用関数の定義
# 地元に残って農業をしない
u_A <- function(P, S){
  return(P + S^(sector1 + sector2))
}

# 地元に残って農業する
u_B <- function(Q, S){
  return(Q^(sector2) + S^(sector1 + sector2))
}

# 地元に残らない
u_C <- function(R){
  return(R)
}

library(froghands)
func <- fhs_create_RD(u_A, u_B, u_C, c(0, 0), c(1, 0), c(2))
fhs_plot_simplex(0.1, 0.3, 0.6)
