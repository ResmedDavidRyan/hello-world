tx_matching_est <- function(horn_scaling, horn_x_pos, horn_y_pos) {
  
  tx_matching_estimate <- -9.7275  + -0.1325*horn_scaling + -2.9200*horn_x_pos + 0.1825*horn_y_pos + 0.1250*horn_scaling*horn_x_pos +
    0.0125*horn_scaling*horn_y_pos + 0.0550 *horn_x_pos*horn_y_pos
  
  result <- tx_matching_estimate
  return(result)
  
}