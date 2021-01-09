#' A formula of three sequences
#' @description That is, element n is the sum of element n−1 and the value of the difference between elements n−3 and n−2 divided by n.
#' @param x input a vector x containing the first three numeric elements of this sequence
#' @param n a positive (>0) integer n denoting the final nth element of the sequence to calculate
#'
#' @return The function should return element n
#' @export myseq_n
#'
#' @examples
#'myseq_n(x = c(2, 3, 3), n = 3)
#'myseq_n(x = c(2, 4, 3), n = 4)
#'myseq_n(x = c(2, 4, 3), n = 5)
#'myseq_n(x = c(2, 4, 3), n = 6)
#'myseq_n(x = c(2, 4, 3), n = 7)
myseq_n <- function(x, n){
  stopifnot(length(x) == 3 & is.numeric(x)) # error check x
  stopifnot(n > 0 & as.integer(n)) # error check n

  nums <- vector(mode = "integer", length = n) # create nums's type
  for(i in seq_along(nums)){
    if(i <= 3){
      nums[i] <- x[i]
    }else{
      nums[i] <- nums[i-1] + (nums[i-3] - nums[i-2])/i
    }
  }
  return(nums[n])
} # created myseq_n function


