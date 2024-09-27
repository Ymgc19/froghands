#' @title define utility function
#' @description \code{fhs_create_RD}
#' @export

# 効用関数を受け取り，レプリケータダイナミクスの勾配をreturn
fhs_create_RD <- function(u_func1, u_func2, u_func3, u_func1_input = c(...), u_func2_input = c(...), u_func3_input){
  RD_fradient <- function(sector1, sector2, sector3){
    # 社会全体の平均利得の計算
    u_mean <- sector1*do.call(u_func1, as.list(u_func1_input)) +
              sector2*do.call(u_func2, as.list(u_func2_input)) +
              sector3*do.call(u_func3, as.list(u_func3_input))
    # それぞれの微分方程式を定義
    d_sector1 <- sector1*(do.call(u_func1, as.list(u_func1_input)) - u_mean)
    d_sector2 <- sector2*(do.call(u_func2, as.list(u_func2_input)) - u_mean)
    d_sector3 <- sector3*(do.call(u_func3, as.list(u_func3_input)) - u_mean)
    return(c(d_sector1, d_sector2, d_sector3, u_mean))
  }
}
