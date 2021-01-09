#' A line graph that plots the relation between first three columns and fourth column.
#' @description The prerequisite is this data frame must have four columns. The first three columns are the values of the three numeric to be input, and the fourth column is the positive integer n for the sequence to be generated.
#' This function should return a line plot of the output values for the different values of n.
#' @param nums A data frame with four columns, and each column must contain numbers only, the fourth column must be the positive integer.
#' @importFrom ggplot2 aes geom_line labs
#' @importFrom tibble tribble as_tibble
#' @return Return a line plot of the output values for the different values of n.
#' @export numSeqPlot
#'
#' @examples
#' # testing the function as data frame
#' my_data <- tibble::tribble(
#' ~x, ~y, ~z, ~n,
#' 2,4,3,3,
#' 2,4,3,4,
#' 2,4,3,5,
#' 2,4,3,6,
#' 2,4,3,7,
#' 2,4,3,8,
#' 2,4,3,9,
#' 2,4,3,10,
#' 2,4,3,12)
#'numSeqPlot(my_data)
numSeqPlot <- function(nums){
  stopifnot(ncol(nums) == 4 & as_tibble(nums)) # error check the length and df type
  stopifnot(all(nums[[4]] > 0) & is.numeric(nums[[1]]) & is.numeric(nums[[2]]),
            is.numeric(nums[[3]]) & as.integer(nums[[4]]))
  # error check the fourth column is a positive integer, and all numbers are numeric

  df <- tibble(n = 0, output = 0) # build a blank data frame
  nums <- tibble(nums) # named nums as tibble
  for (i in 1:nrow(nums)) {
    x <- c(nums[[i, 1]], nums[[i, 2]], nums[[i, 3]]) # extract x, y, z
    n <- nums[[i, 4]] # extract n
    myseq_n(x, n) -> df[i, 2] # the second column
    n -> df[i, 1] #the first column

  }

  df[ ,2] <- round(df[ ,2],digits = 3) # set the rounding numbers will store in ggtitle later
  graphic <- df %>% # make a graphic
    ggplot2::ggplot(mapping = ggplot2::aes(x = n, y = output)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = paste("My Sequence:", df[ ,2]))

  return(graphic) # created numSeqPlot
}

