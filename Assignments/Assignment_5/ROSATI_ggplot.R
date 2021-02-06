data(iris)
library(ggplot2)
library(tidyverse)
ggplot (iris, aes (x = Sepal.Length, y = Sepal.Width, colour = Species)) + stat_density2d ()
ggplot (iris, aes (x = Sepal.Length, y = Sepal.Width, fill = Species)) + stat_binhex (bins=5, aes (alpha = ..count..)) + facet_grid (. ~ Species)
ggplot(iris, aes(x=Sepal.Length, y = Sepal.Width, group = setosa + versicolor + virginica, color = group)) + geom_line()
plot(Petal.Width ~ Petal.Length, data=iris,
     col=c("brown1","dodgerblue1","limegreen")[as.integer(Species)],
     pch=c(1,2,3)[as.integer(Species)])
plot(iris$Species, iris$Sepal.Width)
boxplot(formula = Petal.Length ~ Species, data = iris)


ggplot (iris, aes (x = Sepal.Length, y = Sepal.Width, colour = Species)) + geom_point()
irisplot1 <- ggplot (iris, aes (x = Sepal.Length, y = Petal.Length, colour = Species)) + geom_point() + geom_smooth(method = "lm")
irisplot1

png(filename = "./iris_fig1.png")
irisplot2 <- irisplot1 + labs(title="Sepal Length vs petal length", 
                 subtitle="for three iris species", 
                 y="Petal.Length", 
                 x="Sepal.Length", 
                 caption="") + theme_bw()
dev.off()

irisplot2



hist(iris$Petal.Width)
hist(iris$Petal.Width, breaks=12, col="red")
densityplot <- iris$Petal.Width

densityplot2 <- ggplot(iris, aes(x=Petal.Width)) + geom_density()
densityplot2 <- density(iris$Petal.Width)
densityplot2


hist(iris$Petal.Width)
densityplot3 <- ggplot(data=iris, aes(x=Petal.Width, group=Species, fill=cut)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_bw()
densityplot3

densityplot5 <- ggplot(iris, aes(x = Petal.Width)) +
  geom_density(aes(color = Species)) + lims(x=c(0,2.5),y=c(0,7))
densityplot5



png(filename = "./iris_fig2.png")
densityplot5 <- ggplot(iris, aes(x = Petal.Width, fill=Species)) +
  geom_density(alpha=0.4) + lims(x=c(0,2.5),y=c(0,8)) + labs(title="Distribution of Petal Widths", 
                                                          subtitle="for three iris species", 
                                                          y="Density", 
                                                          x="Petal Width", 
                                                          caption="") + theme_bw()
dev.off()


densityplot5



Irisratio <- iris$Sepal.Width/iris$Petal.Width
Irisratio



png(filename = "./Assignment_5.Rproj iris_fig3.png")
ratioplot <- ggplot(iris, aes(x=Species, y=Petal.Width/Sepal.Width, fill=Species)) + geom_boxplot(color="black") + labs(title="Sepal-to Petal-Width Ratio", 
                                                                                                                        subtitle="for three iris species", 
                                                                                                                        y="Ratio of Sepal Width to Petal Width", 
                                                                                                                        x="Species", 
                                                                                                                        caption="") + theme_bw()
dev.off() #final command code for 3rd graph
ratioplot





iris$Sepal.Length - mean(iris$Sepal.Length)

ggplot(iris, aes(x=Sepal.Length - mean(Sepal.Length), color = Species)) + geom_bar()

devianceplot <- ggplot(iris, aes(x=Sepal.Length - mean(Sepal.Length), color = Species)) + geom_bar()


devianceplot










