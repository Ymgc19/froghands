#' @title define utility function
#' @description \code{fhs_create_RD}
#' @export

# 効用関数を受け取り，レプリケータダイナミクスの勾配をreturn
fhs_create_RD <- function(u_func1, u_func2, u_func3, u_func1_input = c(...), u_func2_input = c(...), u_func3_input){
  RD_fradient <- function(p_sector1, p_sector2, p_sector3){
    # 社会全体の平均利得の計算
    u_mean <- p_sector1*do.call(u_func1, as.list(u_func1_input)) + 
              p_sector2*do.call(u_func2, as.list(u_func2_input)) + 
              p_sector3*do.call(u_func3, as.list(u_func3_input))
    # それぞれの微分方程式を定義
    d_sector1 <- p_sector1*(do.call(u_func1, as.list(u_func1_input)) - u_mean)
    d_sector2 <- p_sector2*(do.call(u_func2, as.list(u_func2_input)) - u_mean)
    d_sector3 <- p_sector3*(do.call(u_func3, as.list(u_func3_input)) - u_mean)
    return(c(d_sector1, d_sector2, d_sector3, u_mean))
  }
}