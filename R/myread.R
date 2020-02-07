#' My csv read function
#'
#' Reads a csv file into the workspace.
#'
#' Given a path string and csv name, reads the csv into a data frame in the workspace.
#'
#' @param csv a string with the filename of the csv to read
#' @param dird a string with the filepath up to the file where the csv is
#'
#' @return a data frame of values
#' @export
#'
#' @examples
#'
#' dird = "[directory path]"; csv = "[filename]"
#' myread(csv, dird)
myread=function(csv,dird){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}
