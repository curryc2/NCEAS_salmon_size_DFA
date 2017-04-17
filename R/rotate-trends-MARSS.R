#' Rotate trends for MARSS Maximum-likelihood model.
#'   Varimax rotation is used to update loadings and trends, to achieve maximum dissimilarity between loadings.
#'
#' @param MLEobj Fitted MARSS ML model
#'
#' @return rotated trends.
#' @export
#'
rotate_trends_MARSS <- function(MLEobj) {
  ## get the estimated ZZ
  Z_est <- coef(MLEobj, type="matrix")$Z
  
  #Get number of state processes
  mm <- dim(Z_est)[2]
  
  if(mm > 1) { #If there are multiple trends estimated and varimax rotation is necessary
    ## get the inverse of the rotation matrix
    H_inv <- varimax(Z_est)$rotmat
  
    ## rotate factor loadings
    Z_rot <- Z_est %*% H_inv   
    ## rotate processes
    proc_rot <- solve(H_inv) %*% MLEobj$states
  }else { #Varimax rotation is unnecessary
    Z_rot <- Z_est
    proc_rot <- MLEobj$states
  }
  #Output Object
  output <- NULL
  output$Z_rot <- Z_rot
  output$proc_rot <- proc_rot
  return(output)
}