#' @title myclt
#'
#' @param n
#' @param iter
#'
#' @return A histogram of a generated uniform sample distribution
#' @export
#'
#' @examples
myclt=function(n,iter){
  y=runif(n*iter,0,5) # A - Creates a uniform sample distribution with a mean of 0 and standard deviation of 5
  data=matrix(y,nr=n,nc=iter,byrow=TRUE) #B - Creates a matrix for the sample data needed for plotting
  sm=apply(data,2,sum) #C - Creates a list of numbers to pull from the data set and sums them
  hist(sm)
  sm
}
w=myclt(n=10,iter=10000) #D - Creates a histogram
