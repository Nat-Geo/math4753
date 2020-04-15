#' My confidence interval generator
#'
#'Finds a 95 percent confidence interval for the mean
#'
#'Given a set of data, computes and returns a 95 percent confidence interval for the mean of that set
#'
#' @param y a vector of data to find a confidence interval on
#'
#' @return ci, a list containing the lower and upper boundaries of the interval
#' @return conflevel, the confidence level of the interval
#' @export
#'
#' @examples
#' set.seed(12); sam1 = rnorm(20, mean = 5, sd = 3); myci(sam1)
myci<-function(y){
  alpha = 0.05
  n = length(y) #length of the input data
  ybar = mean(y) #sample mean
  s = sd(y) #sample variance
  t = qt(1-(alpha/2),n-1) #calculate t_(alpha/2)
  mp = c(-1,1) #represent the plus/minus symbol by calculating for each element of the vector
  mu = ybar + mp*t*s/sqrt(n) #actually calculate each side of the confidence interval
  return(list(ci = mu, conflevel = 1-alpha)) #return values
}
