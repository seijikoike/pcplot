#' Principal Component Plots
#'
#' Takes a data set and plots principal components against each other with ggplot.
#' @param data A set of predictor values
#' @param xpc Principal component to plot along the x-axis
#' @param ypc Principal compenent to plot along y-axis
#' @param cor Indicates to use correlation for PCA. Defaults to TRUE
#' @param control Indicates if user wants to customize ggplot rather than the default setting. Defaults to FALSE.
#' @param group A vector indicating which group the observation belongs to. Defaults to NA.
#' @param labelled Indicates whether or not to label the observations. Defaults to FALSE.
#' @keywords PCA
#' @keywords princomp
#' @examples
#'
#' library(ggplot2)
#' data("USArrests")
#' pcplot(USArrests, 1, 2, labelled = TRUE)
#'
#' data("longley")
#' longley$Group <- ifelse(longley$Year <= 1954, "a", "b")
#' pcplot(longley[,-c(6:8)], 1, 2, group = longley$Group, labelled = TRUE)
#' @export

pcplot <- function(data, xpc, ypc, cor = TRUE, control = FALSE, group = NA, labelled = FALSE){
  pcomp <- princomp(data, cor = cor)
  Ys <- pcomp$scores
  perc <- pcomp$sdev^2

  data <- data.frame(X = Ys[,xpc], Y = Ys[,ypc])

  p <- ggplot(data, aes(X, Y)) +
    xlab(paste("Component ", xpc, "- ", round(perc[xpc]/sum(perc) * 100, 2), "%", sep = "")) +
    ylab(paste("Component ", ypc, "- ", round(perc[ypc]/sum(perc) * 100, 2), "%", sep = "")) +
    ggtitle("Principal Component Graph") +
    theme(legend.position = 'top',
          plot.title = element_text(hjust = 0.5),
          panel.grid.minor = element_line(colour = "grey80"),
          axis.line = element_line(colour = "black"))

  if(control == FALSE){
    if(labelled == TRUE){
      obs <- rownames(data)
      cbind(data, obs)
      if(sum(is.na(group)) == 0){
       p <- p + geom_text(aes(label = obs, colour = factor(group)), vjust = -.8)} +
        labs(colour = "Group: ", text = "Group: ")
      else{p <- p + geom_text(aes(label = obs))}
    }
    if(sum(is.na(group)) == 0){
     cbind(data, group)
     p <- p + geom_point(aes(colour = factor(group))) +
        labs(colour = "Group: ")
    }
    if(sum(is.na(group)) != 0 & labelled == FALSE){
     p <- p + geom_point()
    }
    return(p)
    }
 else{return(p)}
}

