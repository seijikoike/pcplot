#' Scree Plot Function
#'
#' Takes a princomp object and returns a scree plot using ggplot2
#' @param data A set of predictor values
#' @param cor Indicates to use correlation for PCA. Defaults to TRUE
#' @param rounded Rounds the contribution of a PC to a specific number of decimal places. Defaults to 4.
#' @param cumulative Tells scree whether to display the cumulative contribution of PCs or the individual. Defaults to TRUE.
#' @keywords screeplot
#' @keywords princomp
#' @export
#' @examples
#' data(trees)
#' scree(trees, cor = TRUE, rounded = 2)

scree <- function(data, cor = TRUE, rounded = 4, cumulative = TRUE){
  x <- princomp(data, cor = cor)
  vars <- x$sdev^2
  props <- if(cumulative == TRUE) cumsum(x$sdev^2)/sum(x$sdev^2) else x$sdev^2/sum(x$sdev^2)
  comps <- paste("Comp.", 1:ncol(x$loadings))
  df <- data.frame(Variance = vars, Proportion = round(props, rounded), Component = comps)

  p <- ggplot(df, aes(x = Component, y = Variance, group = 1)) +
    geom_line() +
    geom_point(shape = 22, size = 2) +
    geom_text(aes(x = Component, y = Variance, label = Proportion), vjust = -1.3) +
    scale_y_continuous(limits = c(min(df$Variance) - .1*min(df$Variance),
                                  max(df$Variance) + .1*max(df$Variance))) +
    ggtitle("Scree Plot of Principal Components") +
    theme(axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = 0.5))
  return(p)
}
