coupling_est <- function(horn_scaling, horn_x_pos, horn_y_pos) {
  
  coupling_estimate <- -39.02875 + -1.07625*horn_scaling + -0.73125*horn_x_pos + -2.57625*horn_y_pos + -0.91375*horn_scaling*horn_x_pos +
    -0.28875*horn_scaling*horn_y_pos + 2.83625*horn_x_pos*horn_y_pos
  
  result <- coupling_estimate
  return(result)
  
}