myseq_n <- function(x, n){
  stopifnot(length(x) == 3) # error check x
  stopifnot(n > 0) # error check n

  nums <- vector(mode = "integer", length = n) # create nums's type
  for(i in seq_along(nums)) {
    if(i <= 3) {
      nums[i] <- x[i]
    }else{
      nums[i] <- nums[i-1] + (nums[i-3] - nums[i-2]) / i
    }
  }
  return(nums[n])
}# created myseq_n function

# Test function
myseq_n(x = c(2, 3, 3), n = 3)
myseq_n(x = c(2, 4, 3), n = 4)
myseq_n(x = c(2, 4, 3), n = 5)
myseq_n(x = c(2, 4, 3), n = 6)
myseq_n(x = c(2, 4, 3), n = 7)

