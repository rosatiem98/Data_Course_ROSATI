df <- read.csv("../Data/mushroom_growth.csv")

library(tidyverse)
library(modelr)

ggplot(df, aes(x=Nitrogen, y=GrowthRate)) + geom_point()
ggsave("first_plot.png")

ggplot(df, aes(x=Light, y=GrowthRate, color=Species)) + geom_boxplot() 
ggsave("lightgrowthrate_fig.png")

ggplot(df, aes(x=Nitrogen, y=GrowthRate, color=Species)) + geom_point() +
  geom_smooth(method = "lm")

ggplot(df, aes(x=Temperature, y=GrowthRate, color=Species)) + geom_boxplot()

mod1 = lm(GrowthRate ~ Temperature, data=df)
summary(mod1)

mod2 <- lm(GrowthRate ~ Nitrogen, data=df)
summary(mod2)

mean(mod1$residuals^2)

mean(mod2$residuals^2)

mod3 = lm(GrowthRate ~ Temperature + Nitrogen, data=df)

mean(mod3$residuals^2)

mod4 = lm(GrowthRate ~ Temperature*Nitrogen, data=df)

mean(mod4$residuals^2)

mod5 = lm(GrowthRate ~ Light + Temperature, data=df)

mean(mod5$residuals^2)

mod6 = lm(GrowthRate ~ Temperature*Light, data=df)

mean(mod6$residuals^2)

mod7 <- lm(GrowthRate ~ Temperature + Light + Nitrogen, data=df)

mean(mod7$residuals^2)

ggplot(df, aes(x=Humidity, y=GrowthRate, color=Species)) + geom_boxplot()


mod8 = lm(GrowthRate ~ Humidity + Temperature + Light, data=df)

mean(mod8$residuals^2)

mod9 = lm(GrowthRate ~ Humidity*Temperature*Light, data=df)

mean(mod9$residuals^2)

# Keep mod9
newdfl <- data.frame(Temperature = c(25,30,35,40),Light = c(30,35,40,45),Humidity = c("medium"))

pred = predict(mod6, newdata = newdfl)

hyp_predsl <- data.frame(Temperature = newdfl$Temperature, Light = newdfl$Light, Humidity=newdfl$Humidity,
                         pred = pred)

df$PredictionType <- "Real"
newdfl$PredictionType <- "Hypothetical"

full_predsl <- full_join(df,hyp_predsl)

lmtemp <- ggplot(full_predsl,aes(x=Temperature,y=pred,color=PredictionType)) +
  geom_point() +
  geom_point(aes(y=GrowthRate),color="Blue") +
  theme_minimal()

lmlight <- ggplot(full_predsl,aes(x=Light,y=pred,color=PredictionType)) +
  geom_point() +
  geom_point(aes(y=GrowthRate),color="Blue") +
  theme_minimal()

lmhum <- ggplot(full_predsl,aes(x=Humidity,y=pred,color=PredictionType)) +
  geom_point() +
  geom_point(aes(y=GrowthRate),color="Blue")
  theme_minimal()


ggarrange(lmhum,lmtemp,lmlight)

# Anova Tests

df2 <- df %>%
  add_predictions(mod9)

amod1 = aov(GrowthRate~Nitrogen, data=df)

summary(amod1)

mean(amod1$residuals^2)

amod2 = aov(GrowthRate~Temperature, data=df)

summary(amod2)

mean(amod2$residuals^2)

amod3 = aov(GrowthRate~Temperature + Humidity, data=df)

summary(amod3)

mean(amod3$residuals^2)

amod4 = aov(GrowthRate~Temperature*Humidity, data=df)

summary(amod4)

mean(amod4$residuals^2)

amod5 = aov(GrowthRate~Temperature + Nitrogen + Humidity + Light + Species, data=df)

summary(amod5)

mean(amod5$residuals^2)

amod6 = aov(GrowthRate~Humidity + Species + Light, data=df)

summary(amod6)

mean(amod6$residuals^2)

amod7 = aov(GrowthRate~Humidity * Light, data=df)

mean(amod7$residuals^2)

# How to make predictions with categorical data like humidity?
amod8 = aov(GrowthRate~Light, data=df)

summary(amod8)

mean(amod8$residuals^2)

df3 <- df %>%
  add_predictions(amod8)
df3[,c("Light","pred")] %>% head()


newdf3 <- data.frame(Light=c(25,30,35,40))

pred=predict(amod8, newdata = newdf3)
hyp_preds <- data.frame(Light = newdf3$Light,
                        pred=pred)


df$PredictionType <- "Real"
hyp_preds$PredictionType <- "Hypothetical"

fullpredsl <- full_join(df,hyp_preds)


Light <- ggplot(fullpredsl, aes(x=Light, y=pred, color=PredictionType)) +
  geom_point() + 
  geom_point(aes(y=GrowthRate))

# workflow of graphing and modeling

amod9 <- aov(GrowthRate~Light:Temperature, data=df)
summary(amod9)

mean(amod9$residuals^2)


amod10 <- aov(GrowthRate~Temperature, data=df)

summary(amod10)

mean(amod10$residuals^2)



amod11 <- aov(GrowthRate~Temperature*Light*Humidity, data=df)

mean(amod11$residuals^2)

# Graph of model amod10
df4 <- df %>%
  add_predictions(amod9)
df4[,c("Temperature","pred")] %>% head()


newdf4 <- data.frame(Temperature = c(30,35,40,45), Light = c(25,30,35,40))

pred = predict(amod9, newdata = newdf4)
hyp_preds2 <- data.frame(Temperature = newdf4$Temperature,
                        pred=pred)
df$PredictionType <- "Real"
hyp_preds2$PredictionType <- "Hypothetical"
full_preds2 <- full_join(df,hyp_preds2)

Temp <- ggplot(full_preds2, aes(x=Temperature, y=pred, color=PredictionType)) +
  geom_point() + 
  geom_point(aes(y=GrowthRate))

library(ggpubr)
ggarrange(Temp,Light)


adf <- df %>%
  add_predictions(amod11)
adf[,c("GrowthRate","pred")] %>% head()

newadf <- data.frame(Temperature = c(25,30,35,40),Light = c(30,35,40,45),Humidity = c("medium"))

pred = predict(amod11,newdata = newadf)

ahyp_preds <- data.frame(Temperature = newadf$Temperature,Light=newadf$Light,Humidity=newadf$Humidity,
                         pred=pred)
df$PredictionType <- "Real"
ahyp_preds$PredictionType <- "Hypothetical"
afull_preds <- full_join(df,ahyp_preds)



Temp <- ggplot(afull_preds, aes(x=Temperature, y=pred, color=PredictionType)) +
  geom_point() + 
  geom_point(aes(y=GrowthRate))


Light <- ggplot(afull_preds, aes(x=Light, y=pred, color=PredictionType)) +
  geom_point() + 
  geom_point(aes(y=GrowthRate))

Hum <- ggplot(afull_preds, aes(x=Humidity, y=pred, color=PredictionType)) +
  geom_point() + 
  geom_point(aes(y=GrowthRate))

ggarrange(Temp,Light,Hum)

adf2 <- df %>%
  add_predictions(amod5)
adf[,c("GrowthRate","pred")] %>% head()

newadf2 <- data.frame(Temperature = c(25,30,35,40),Light = c(30,35,40,45),Humidity = c("medium"), Nitrogen = c(50,55,60,65), Species = c("C.cibarius"))

pred = predict(amod5,newdata = newadf2)

ahyp_preds2 <- data.frame(Temperature = newadf$Temperature,Light=newadf$Light,Humidity=newadf$Humidity, Nitrogen=newadf2$Humidity,Species=newadf2$Species,
                         pred=pred)
df$PredictionType <- "Real"
ahyp_preds$PredictionType <- "Hypothetical"
afull_preds2 <- full_join(df,ahyp_preds)



Temp2 <- ggplot(afull_preds2, aes(x=Temperature, y=pred, color=PredictionType)) +
  geom_point() + 
  geom_point(aes(y=GrowthRate))


Light2 <- ggplot(afull_preds2, aes(x=Light, y=pred, color=PredictionType)) +
  geom_point() + 
  geom_point(aes(y=GrowthRate))

Hum2 <- ggplot(afull_preds2, aes(x=Humidity, y=pred, color=PredictionType)) +
  geom_point() + 
  geom_point(aes(y=GrowthRate))

Species <- ggplot(afull_preds2, aes(x=Species, y=pred, color=PredictionType)) +
  geom_point() + 
  geom_point(aes(y=GrowthRate))

Nitro <- ggplot(afull_preds2, aes(x=Nitrogen, y=pred, color=PredictionType)) +
  geom_point() + 
  geom_point(aes(y=GrowthRate))

ggarrange(Temp2,Light2,Hum2,Nitro,Species)




nondf <- read.csv("../Data/non_linear_relationship.csv")


ggplot(nondf, aes(x=predictor, y=response)) + geom_point()

exponential.model <- lm(log(response)~predictor, data=nondf)
summary(exponential.model)

mean(exponential.model$residuals^2)
