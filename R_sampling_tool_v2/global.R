require(plyr)
require(dplyr)
require(car)
require(reshape2)
require(stringr)
library(shiny)
library(shinyjs)


Ssize<-function (x,A,p,E) {(qchisq(A,1)*x*p*(1-p)) / (E^2*(x-1)+qchisq(A,df=1)*p*(1-p))}
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
