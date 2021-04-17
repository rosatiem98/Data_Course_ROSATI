library(tidyr)
library(tidyverse)
library(dplyr)
library(plyr)
library(janitor)
library(broom)
library(modelr)
library(fitdistrplus)
#Task 1

df <- read.csv("./FacultySalaries_1995.csv")

cleandf <- clean_names(df)

sep_df <- subset(cleandf, select = -c(fed_id, univ_name,state))
sep_df2 <- subset(sep_df, select = -c(5:14))

names(seq_df2) = gsub(pattern="avg_",replacement = "", x= names(sep_df2))

piv_df <- sep_df2 %>%
  pivot_longer(!tier, names_to ="Rank", values_to = "Salary") %>%
  mutate(Rank = Rank %>% str_remove_all("avg_")) %>%
  mutate(Rank = Rank %>% str_remove_all("_prof_salary")) %>%
  filter(tier != "VIIB") %>%
  mutate(Rank=Rank %>% str_replace_all("a","A")) %>%
  mutate(Rank=Rank %>% str_replace_all("f","F"))


tsk1plot <- ggplot(data = piv_df, aes(x=Rank, y=Salary, fill=Rank))+
  geom_boxplot()+
  facet_wrap(~tier) + theme_minimal()

p1 <- tsk1plot + theme(axis.text.x = element_text(angle = 45))
p1
ggsave(plot=p1, "Rosati_Fig_1.jpg", scale = 1.2, width=3)

#Task 2


tsk2 <- subset(cleandf, select = -c(1:2,8:17)) %>%
  pivot_longer(starts_with("avg"),names_to = "Rank", values_to = "Salary" ) %>%mutate(Rank = Rank %>% str_remove_all("avg_")) %>%
  mutate(Rank = Rank %>% str_remove_all("_prof_salary"))
  
tsk2mod <- aov(Salary~state + Rank + tier, data=tsk2)
summary(tsk2mod)
capture.output(summary(tsk2mod), file="Salary_Anova_Summary.txt")


#Task 3
df2 <- read.csv("./Juniper_Oils.csv")
cleandf2 <- clean_names(df2)

df2_select <- subset(cleandf2,select = -c(1:10))

df2_select2 <- subset(df2_select, select= -c(24:30)) %>%
  pivot_longer(!years_since_burn, names_to = "ChemicalID", values_to = "concentration") %>%
  mutate(ChemicalID = ChemicalID %>% str_replace_all("_","-"))
colnames(df2_select2)[1] <- "YearsSinceBurn"
  

#Task 4

p2 <- ggplot(df2_select2, aes(x=YearsSinceBurn, y=concentration)) +
  facet_wrap(~ChemicalID, scales = "free") + geom_smooth(size=1.5) +
  theme_minimal()
ggsave(plot = p2,"Rosati_Fig_2.jpg", scale=2, width = 4, height = 2.5,dpi=300)

#Task 5
chemmod1 <- glm(concentration~ ChemicalID*YearsSinceBurn, data=df2_select2)
summary(chemmod1)

gf <- tidy(chemmod1)

finaltable <- subset(gf, p.value < 0.05) %>%
  mutate(term = term %>% str_remove_all("ChemicalID"))
finaltable
