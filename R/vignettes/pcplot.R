## ----fig.align = 'center', fig.height = 4, fig.width = 5-----------------
data("iris")
head(iris)

## ----fig.align = 'center', fig.height = 4, fig.width = 5-----------------
library(pcplot)
library(ggplot2)

pcplot(iris[, -5], 1, 2)

## ----fig.align = 'center', fig.height = 4, fig.width = 5-----------------
pcplot(iris[, -5], 4, 3)

## ----fig.align = 'center', fig.height = 4, fig.width = 5-----------------
irispc <- princomp(iris[, -5], cor = TRUE)
irispc$sdev^2

## ----fig.align = 'center', fig.height = 4, fig.width = 5-----------------
irispc$sdev^2 / sum(irispc$sdev^2)

## ----fig.align = 'center', fig.height = 4, fig.width = 5-----------------
iriscov <- princomp(iris[, -5])
iriscov$sdev^2 / sum(iriscov$sdev^2)

## ----fig.align = 'center', fig.height = 4, fig.width = 5-----------------
pcplot(iris[, -5], 1, 2, cor = FALSE)

## ----fig.align = 'center', fig.height = 4, fig.width = 5-----------------
pcplot(iris[, -5], 1, 2, group = iris$Species)

## ----fig.align = 'center', fig.height = 4, fig.width = 5-----------------
pcplot(iris[, -5], 1, 2, group = iris$Species, labelled = TRUE)

## ----fig.align = 'center', fig.height = 4, fig.width = 5-----------------
data("USArrests")
pcplot(USArrests, 1, 2, labelled = TRUE)

## ----fig.align = 'center', fig.height = 4, fig.width = 5-----------------
irisplot <- pcplot(iris[, -5], 1, 2, control = TRUE)
irisplot

## ----fig.align = 'center', fig.height = 4, fig.width = 5-----------------
irisplot + geom_point()

## ----fig.align = 'center', fig.height = 4, fig.width = 5-----------------
irisplot <- irisplot + geom_point(aes(shape = iris$Species, colour = iris$Species))
irisplot

## ----fig.align = 'center', fig.height = 4, fig.width = 5-----------------
irisplot <- irisplot + 
  labs(title = "PC Plot of Iris Data", colour = "Species", shape = "Species") + 
  scale_colour_discrete(labels = c("Setosa", "Veriscolor", "Virginica")) + 
  scale_shape_discrete(labels = c("Setosa", "Veriscolor", "Virginica"))
irisplot

## ----fig.align = 'center', fig.height = 4, fig.width = 5-----------------
irisplot <- irisplot +
  scale_x_continuous(breaks = seq(-3, 3, 1))
irisplot

## ----fig.align = 'center', fig.height = 4, fig.width = 5-----------------
irisplot <- irisplot + 
  theme(panel.grid.minor = element_line(colour = "grey80"),
        panel.background = element_rect(fill = "lightblue"),
        axis.title = element_text(size = 18, colour = "red"),
        plot.title = element_text(colour = "sienna4"))
irisplot

## ----fig.align = 'center', fig.height = 4, fig.width = 5-----------------
irisplot <- irisplot +
  scale_colour_manual(values = c("orange", "purple", "orangered2"),
                      labels = c("Setosa", "Versicolor", "Virginica")) + 
  scale_shape_manual(values = c(17, 19, 24),
                     labels = c("Setosa", "Versicolor", "Virginica"))
irisplot

## ----eval = FALSE--------------------------------------------------------
#  irisplot <- pcplot(iris[, -5], 1, 2, control = TRUE)
#  irisplot +
#    geom_point(aes(shape = iris$Species, colour = iris$Species)) +
#    labs(title = "PC Plot of Iris Data", colour = "Species", shape = "Species") +
#    scale_x_continuous(breaks = seq(-3, 3, 1)) +
#    theme(panel.grid.minor = element_line(colour = "grey80"),
#          panel.background = element_rect(fill = "lightblue"),
#          axis.title = element_text(size = 18, colour = "red"),
#          plot.title = element_text(colour = "sienna4")) +
#    scale_colour_manual(values = c("orange", "purple", "orangered2"),
#                        labels = c("Setosa", "Versicolor", "Virginica")) +
#    scale_shape_manual(values = c(17, 19, 24),
#                       labels = c("Setosa", "Versicolor", "Virginica"))

