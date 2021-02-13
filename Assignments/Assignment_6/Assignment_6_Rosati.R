data("mtcars")
mtcars$am == 0
my_subset <- mtcars[mtcars$am == 0,]
write.csv(my_subset, "automatic_mtcars.csv")
library(tidyverse)
ggplot(my_subset, aes(x=hp, y=mpg)) + geom_point() +
  geom_smooth() + labs(title="Mpg vs. Hp", 
subtitle= "with Automatic Transmission",
y= "Mpg",
x= "Hp",
caption="")
ggsave("mpg_vs_hp_auto.png")


ggplot(my_subset, aes(x=wt, y=mpg)) + geom_point() +
  geom_smooth() + labs(title="Mpg vs. Weight", 
                      subtitle= "with Automatic Transmission",
                      y= "Mpg",
                      x= "Weight (lbs.)",
                      caption="")
ggsave("mpg_vs_wt_auto.tiff")

disp200 <- mtcars[mtcars$disp <= 200,]
write.csv(disp200, "mtcars_max200_displ.csv")


maxmtcars <- max(mtcars$hp)
maxauto <- max(my_subset$hp)
maxdisp200 <- max(disp200$hp) #Turn into a txt file



p1 <- ggplot(mtcars, aes(x=wt, y=mpg, color=cyl)) + geom_point() + geom_smooth(method=lm) +
  labs (title="Mpg vs. Weight", 
   subtitle= "of all observations w/ # of cylinders",
   y= "Mpg",
   x= "Weight (lbs.)",
   caption="")
mtcars$cyl <- as.factor(mtcars$cyl)

p2 <- ggplot(mtcars, aes(x=cyl, y=mpg, color=cyl)) + geom_violin() + 
  labs (title="Mpg vs. #of cylinders", 
        subtitle= "of all observations",
        y= "Mpg",
        x= "# of cylinders",
        caption="")

p3 <- ggplot(mtcars, aes(x=hp, y=mpg, color=cyl)) + geom_point() + geom_smooth(method =lm) +
  labs (title="Mpg vs. Hp",
        subtitle="w/ # of cylinders",
        y="Mpg",
        x="Horse Power",
        caption="")
install.packages("patchwork")  
library(patchwork)
patchwork <- (p1 + p2 + p3)
patchwork + plot_annotation(title = "Factors that affect Mpg in mtcars dataset",
                            subtitle = "These plots will reveal truth about Mpg") 
ggsave("combined_mtcars_plot.png", width = 8, height=5, units="in")
