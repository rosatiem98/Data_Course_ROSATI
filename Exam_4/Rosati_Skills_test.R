
#I will be re-doing exam 1 

# Task 1 
read.csv("./DNA_Conc_by_Extraction_Date.csv")
df <- read.csv("./DNA_Conc_by_Extraction_Date.csv")
library(tidyverse)
ggplot(df, aes(x=DNA_Concentration_Katy)) +  geom_histogram() + labs(title="DNA Concentration", 
                                                                     subtitle="for Katy", 
                                                                     y="Number of Extractions", 
                                                                     x="DNA Concentration", 
                                                                     caption="") + theme_minimal()
ggplot(df, aes(x=DNA_Concentration_Ben)) + geom_histogram() + labs (title="DNA Concentration", 
                                                                    subtitle="for Ben", 
                                                                    y="Number of Extractions", 
                                                                    x="DNA Concentration", 
                                                                    caption="") + theme_minimal()
#Task 2
Year_Collectedfac <- as.factor(df$Year_Collected)
ggplot(df, aes(x=Year_Collectedfac, y=DNA_Concentration_Katy,)) + 
  stat_boxplot(geom="errorbar", linetype=1, width=0.5) + 
  geom_boxplot(outlier.shape=1) +theme_classic()+labs (title="Katy's Extractions", 
                                                        subtitle="", 
                                                        y="DNA Concentration", 
                                                        x="Year Collected", 
                                                        caption="") + theme_classic()  
fac <- as.factor(df$Year_Collected)
ggplot(df, aes(x=Year_Collectedfac, y=DNA_Concentration_Ben,)) + 
  stat_boxplot(geom='errorbar', linetype=1, width=0.5) + 
  geom_boxplot(outlier.shape=1) + theme_classic() + labs (title="Ben's Extractions", 
                                                          subtitle="", 
                                                          y="DNA Concentration", 
                                                          x="Year Collected", 
                                                          caption="") + theme_classic()
#Task 3
Year_Collectedfac <- as.factor(df$Year_Collected)
g1 <- ggplot(df, aes(x=Year_Collectedfac, y=DNA_Concentration_Katy,)) + 
  stat_boxplot(geom='errorbar', linetype=1, width=0.5) + 
  geom_boxplot(outlier.shape=1) + theme_classic()+labs (title="Katy's Extractions", 
                                                        subtitle="", 
                                                        y="DNA Concentration", 
                                                        x="Year Collected", 
                                                        caption="") + theme_classic()  
g1 + ggtitle("Katy's plot") + theme(plot.title = element_text(hjust = 0.5))
ggsave("./Rosati_Plot1.jpeg", width=6)








Year_Collectedfac <- as.factor(df$Year_Collected)
g2 <- ggplot(df, aes(x=Year_Collectedfac, y=DNA_Concentration_Ben,)) + 
  stat_boxplot(geom='errorbar', linetype=1, width=0.5) + 
  geom_boxplot(outlier.shape=1) + theme_classic() + labs (title="Ben's Extractions", 
                                                          subtitle="", 
                                                          y="DNA Concentration", 
                                                          x="Year Collected",caption="") + theme_classic()
g2 + ggtitle("Ben's plot") + theme(plot.title = element_text(hjust = 0.5))
ggsave("./Rosati_Plot2.jpeg", width=6)

#Task 4

Ben <- df[c(1,3,5)]
Kate <- df[c(1,3:4)]

min(Ben$DNA_Concentration_Ben)
min(Kate$DNA_Concentration_Katy)

median(Ben$DNA_Concentration_Ben)
median(Kate$DNA_Concentration_Katy)

Kate1 <- Kate %>%
  filter(DNA_Concentration_Katy <= 1 )
Ben1 <- Ben %>%
  filter(DNA_Concentration_Ben <= 1)

max(Kate1$DNA_Concentration_Katy)
max(Ben1$DNA_Concentration_Ben)

which(Kate1$DNA_Concentration_Katy >= 0.982, arr.ind = TRUE)

Kate1$Year[[133]]

which(Ben1$DNA_Concentration_Ben >= 0.996505, arr.ind = TRUE)

Ben1$Year[[40]]


#Task 5
labdata <- df[df$Lab == "Downstairs",]

timeDate <- as.POSIXct(labdata$Date_Collected)

ggplot(labdata, aes(x=timeDate, y=DNA_Concentration_Ben)) + geom_point(shape=1, size=2) + labs (title="DNA Concentration/Year", 
                                                                                               subtitle="for Ben", 
                                                                                               y="DNA Concentration", 
                                                                                               x="Year Collected", 
                                                                                               caption="") + theme_classic()
ggsave("Ben_DNA_over_time.jpg")

#task 6

mn2000 <- with(df, mean(DNA_Concentration_Ben[Year_Collected == 2000]))
mn2001 <- with(df, mean(DNA_Concentration_Ben[Year_Collected == 2001]))
mn2002 <- with(df, mean(DNA_Concentration_Ben[Year_Collected == 2002]))
mn2003 <- with(df, mean(DNA_Concentration_Ben[Year_Collected == 2003]))
mn2004 <- with(df, mean(DNA_Concentration_Ben[Year_Collected == 2004]))
mn2005 <- with(df, mean(DNA_Concentration_Ben[Year_Collected == 2005]))
mn2006 <- with(df, mean(DNA_Concentration_Ben[Year_Collected == 2006]))
mn2007 <- with(df, mean(DNA_Concentration_Ben[Year_Collected == 2007]))
mn2008 <- with(df, mean(DNA_Concentration_Ben[Year_Collected == 2008]))
mn2010 <- with(df, mean(DNA_Concentration_Ben[Year_Collected == 2010]))
mn2011 <- with(df, mean(DNA_Concentration_Ben[Year_Collected == 2011]))
mn2012 <- with(df, mean(DNA_Concentration_Ben[Year_Collected == 2012]))

Year <- c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2010","2011","2012")
Mean_dna_conc <- c(0.4844,0.5428,0.5651,0.4378,0.7310,1.2838,1.2198,1.4633,1.1849,1.1838,1.1206,1.0921)
Benaverage <- data.frame(Year,Mean_dna_conc)

max(Benaverage$Mean_dna_conc)

which(Benaverage$Mean_dna_conc == 1.4633, arr.ind = TRUE)    
Benaverage$Year[[8]]
write.csv(x=Benaverage, file="Ben_Average_Conc.csv")
