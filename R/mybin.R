#' My binary distribution generator
#'
#' @param iter a number of simulations to run
#' @param n a number of decisions to make in each iterations
#' @param p a number probability that each decision is a success
#'
#' @return a table with the probability for each number of success in 0:n
#' @export
#'
#' @examples
#' probTable<-mybin(iter=10000,n=10,p=0.7)
mybin=function(iter=100,n=10, p=0.7){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
