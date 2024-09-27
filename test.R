# 効用関数の定義
# 地元に残って農業をしない
u_A <- function(sector1, sector2, P, S){
  return(P + S^(sector1 + sector2))
}

# 地元に残って農業する
u_B <- function(sector1, sector2, Q, S){
  return(Q^(sector2) + S^(sector1 + sector2))
}

# 地元に残らない
u_C <- function(R){
  return(R)
}

