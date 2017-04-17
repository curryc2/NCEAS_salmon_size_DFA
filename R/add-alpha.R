#' Change transparency of colors
#'
#' @param cols Vector of colors to be transparentified
#' @param alpha Transparency Value
#'
#' @return
#' @export
#'
#' @examples
add_alpha <- function(cols, alpha=1){
  if(missing(cols))
    stop("Please provide a vector of colours.")
  apply(sapply(cols, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}