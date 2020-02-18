#' My normal distribution plotter
#'
#' Plots a normal distribution with the given parameters
#'
#' Given the mean, standard deviation and a value to evaluate against, plots the curve and shades the lower tail, then returns the probability for a value to be in the lower tail.
#'
#' @param mu a number, the mean of the normal distribution
#' @param sigma a number, the standard deviation of the normal distribution
#' @param a a number, the point up to which to calculate the lower tail
#'
#' @return a plot with the lower tail shaded and a list containing the probability for a value to be in that area
#' @export
#'
#' @examples
#' myncurve(mu=10,sigma=5,a=6)
myncurve = function(mu, sigma,a){
  # draw the curve
  xmin=mu-3*sigma
  curve(dnorm(x,mean=mu,sd=sigma),xlim=c(xmin,mu+3*sigma))

  # shade the area

  xcurve=seq(xmin,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(xmin,xcurve,a),c(0,ycurve,0),col="blue")

  # calculate the probability
  prob=pnorm(a,mean=mu,sd=sigma)
  #relase it to the command line
  prob
}
