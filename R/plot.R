#' Plot one-way ANOVA
#'
#' plot.oneway creates group comparisons for a one-way ANOVA
#'
#' @param x an object of class oneway
#' @param ... additional arguments passed to geom boxplot function
#'
#' @return NULL
#' @export
#' @return ggplot2 graph
#' @examples
#' mileage <- oneway(mpg~cyl, mtcars)
#' plot(mileage)
plot.oneway <- function(x, ...){
  if(!inherits(x, "oneway"))
    stop("Must be class 'oneway'")
  xvar <- as.character(x$anova$terms[[3]])
  yvar <- as.character(x$anova$terms[[2]])
  data <- x$anova$model
  ggplot(data, aes(x = factor(.data[[xvar]]), y= .data[[yvar]]))+
    geom_boxplot(...)+
    labs(x = xvar)

}

