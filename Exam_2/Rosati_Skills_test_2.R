library(tidyverse)
library(dplyr)
df <- read.csv("./landdata-states.csv")
df2 <- read.csv("./unicef-u5mr.csv")

#Plot 1
ggplot(df, aes(x=Year,y = Land.Value, color=region)) + geom_smooth() +
  labs (title="", 
        subtitle="", 
        y="Land Value (USD)", 
        x="Year", 
        caption="")


ggsave("Rosati_fig_1.jpg", width = 5)
  
#Task 2
# The N/A is representing is the District of Columbia

DC <- subset(df, State == "DC")

#Task 3
library(janitor)
names(df2) <- names(df2) %>% make_clean_names()

names(df2) = gsub(pattern = "u5mr_", replacement = "", x= names(df2))

newdf2

df2s <- subset(df2, select = -c(country_name, region))
df2_sep <- df2s %>%
  pivot_longer(!continent, names_to = "Year", values_to = "MortalityRate")

Morplot <- ggplot(df2_sep, aes(x=factor(Year), y=MortalityRate, color=continent)) + geom_point() +
  scale_x_discrete(breaks = c(1960, 1980, 2000))
  

#Task 4
ggsave("Rosati_fig_2.jpg", width = 5)

#Task5
df2n <- na.omit(df2)
newdf2c <- subset(df2n, select = -c(region))

newdf2c_sep <- newdf2c %>%
  pivot_longer(!continent & !country_name, names_to = "Year", values_to = "MortalityRate")

df3 <- newdf2c %>% 
  group_by(Year, Continent) %>%
  summarise(mean_MortalityRate = mean(MortalityRate))



#Task 6

newdf2r <- subset(df2, select = -c(country_name, continent))

newdf2r

newdf2r_sep <- newdf2r %>%
  pivot_longer(!region, names_to = "Year", values_to = "MortalityRate")

DT <- newdf2r_sep[, list(props=sum("MortalityRate") / .N, count=.N), by=region]

ggplot(newdf2r_sep, aes(x=Year, y=MortalityRate/1000)) + geom_point(color = "blue", alpha=0.35) +
  facet_wrap(~region) +  scale_x_discrete(breaks = c(1960, 1980, 2000)) + theme_update()
  labs (title="", 
               subtitle="", 
               y="Mortality Rate", 
               x="Year", 
               caption="")

ggsave("Rosati_fig_4.jpg", width =10, height=8)
