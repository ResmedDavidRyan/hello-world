gain_est <- function(horn_scaling, horn_x_pos, horn_y_pos) {
  
   gain_estimate <- 2.7825 + 0.7825*horn_scaling + 0.0650*horn_x_pos + -0.0225*horn_y_pos + -0.1000*horn_scaling*horn_x_pos +
     0.0625*horn_scaling*horn_y_pos + 0.02*horn_x_pos*horn_y_pos
   
   result <- gain_estimate
   return(result)
  
}