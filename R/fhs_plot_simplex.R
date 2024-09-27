#' @title define utility function
#' @description \code{fhs_plot_simplex}
#' @export


fhs_plot_simplex <- function(RD_func, arrow_interval = 25, utility_interval = 100, arrow_shortness = 8){
  library(tidyverse)
  # 正三角形を描画するためのpoint
  x <- c(0, 1, 0.5)
  y <- c(0, 0, sqrt(3)/2)
  df <- tibble(x, y)
  
  # ========================================== #
  # ========== 矢印の方向を計算する ========== #
  # ========================================== #
  arrow_points <- froghands::fhs_generate_simplex_points(arrow_interval)
  arrow_x <- arrow_points$x
  arrow_y <- arrow_points$y
  df_arrow <- tibble(arrow_x, arrow_y)
  
  # それぞれの矢印の始点における勾配を計算
  d_sector1_vec <- c()
  d_sector2_vec <- c()
  d_sector3_vec <- c()
  # 勾配計算
  for (i in 1:nrow(df_arrow)){
    # 処理するデータ
    hoge_2d <- df_arrow[i,] %>% as.numeric()
    # 3次元に変換
    hoge_3d <- convert_3d(hoge[1], hoge[2])
    # 微分係数の計算
    result <- RD_func(hoge_3d[1] %>% as.numeric, 
                      hoge_3d[2] %>% as.numeric, 
                      hoge_3d[3] %>% as.numeric)
    d_sector1_vec <- c(d_sector1_vec, result[1]) # 地元で農業をしない方向への勾配
    d_sector2_vec <- c(d_sector2_vec, result[2]) # 地元で農業をする方向への勾配
    d_sector3_vec <- c(d_sector3_vec, result[3]) # 他の地域へ移住する場合の勾配
  }
  # データフレーム化する
  df_move <- tibble(
    d_sector1 = d_sector1_vec, 
    d_sector2 = d_sector2_vec, 
    d_sector3 = d_sector3_vec
  )
  
  # 3次元の勾配を2次元に変換
  v1 <- c(1, 0)  # 頂点1
  v2 <- c(0.5, sqrt(3)/2)  # 頂点2
  v3 <- c(0, 0)  # 頂点3
  # 2次元に変換した数値を入れていく
  move_x <- c()
  move_y <- c()
  for (i in 1:nrow(result_df)){
    hoge <- df_move[i,] %>% as.numeric()
    projected_2d <- hoge[1] * v1 + hoge[2] * v2 + hoge[3] * v3
    move_x <- c(move_x, projected_2d[1])
    move_y <- c(move_y, projected_2d[2])
  }
  # データフレームに勾配方向を追加
  df_arrow$move_x <- move_x
  df_arrow$move_y <- move_y
  
  # ============================================ #
  # ======= 平均効用で背景を塗るための数値 ===== #
  # ============================================ #
  mean_points <- generate_points(utility_interval)
  # 矢印の座標を作る
  point_x <- mean_points$x
  point_y <- mean_points$y
  df_mean_points <- tibble(point_x, point_y)
  df_mean_points %>% glimpse()
  
  # それぞれの地点における平均効用を計算
  u_mean_vec <- c()
  # 平均効用計算
  for (i in 1:nrow(df_mean_points)){
    # 処理するデータ
    hoge_2d <- df_mean_points[i,] %>% as.numeric()
    # 3次元に変換
    hoge_3d <- convert_3d(hoge[1], hoge[2])
    # 微分係数の計算
    result <- RD_func(hoge_3d[1] %>% as.numeric, 
                      hoge_3d[2] %>% as.numeric, 
                      hoge_3d[3] %>% as.numeric)
    u_mean_vec <- c(u_mean_vec, result[4])
  }
  df_mean_points$u_mean <- u_mean_vec
  
  # ============================================ #
  # ======= ここからシンプレクスを描画する ===== #
  # ============================================ #
  # 微分係数が0のところを抜き出す．矢印ではなく丸印を打つため
  df_move_zero <- df_arrow %>% 
    filter(
      (abs(move_x) <= .00001) & (abs(move_y) <= .00001)
    )
  
  # =================== 描画 =================== #
  simplex <- ggplot() +
    # 効用の平均値で色塗り
    geom_point(
      data = df_mean_points,
      aes(x = point_x, y = point_y, color = u_mean),
      size = 5.1, shape = 17, alpha = 1
    ) +
    # 正三角形の描画
    geom_polygon(
      data = df,
      aes(x, y), fill = NA, color = "black"
    ) +
    scale_color_gradient(low = "#19d", high = "salmon") +
    # 矢印の描画
    geom_segment(
      data = df_arrow,
      aes(x = arrow_x, y = arrow_y, 
          xend = arrow_x + move_x/arrow_shortness, yend = arrow_y + move_y/arrow_shortness), 
      arrow = arrow(length = unit(0.05, "inches")), 
      color = "white", size = .5, lwd = .5
    ) + 
    # 3頂点にpoint
    geom_point(
      data = df_move_zero,
      aes(arrow_x, arrow_y), color = "white", size = 3
    ) +
    # そのほか設定
    theme_classic() +
    labs(x = "", y = "", fill = "", color = "") +
    theme(aspect.ratio= .9,
          text = element_text(family = "Times New Roman"),
          legend.position = "none",
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank())
  
  # 出力
  return(simplex = simplex)
}
