#ellipse equation
ellipse_func <- function(x_mean,y_mean,x_sd,y_sd,step=1){
  y <- seq(y_mean-y_sd,y_mean+y_sd,step)
  x <- x_mean+sqrt((1-((y-y_mean)^2/(y_sd^2)))*(x_sd^2))
  x2 <- x_mean-sqrt((1-((y-y_mean)^2/(y_sd^2)))*(x_sd^2))
  return(data.frame(x=c(x,x2[order(-y)]),
                    y=c(y,y[order(-y)])))
}
