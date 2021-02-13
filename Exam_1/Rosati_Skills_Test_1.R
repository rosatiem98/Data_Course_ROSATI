#Task 1

read.csv("./DNA_Conc_by_Extraction_Date.csv")
df <- read.csv("./DNA_Conc_by_Extraction_Date.csv")
library(tidyverse)
ggplot(df, aes(x=DNA_Concentration_Katy)) +  geom_histogram() + labs(title="DNA Concentration", 
                                                                     subtitle="for Katy", 
                                                                     y="Count", 
                                                                     x="DNA Concentration", 
                                                                     caption="") + theme_minimal()
ggplot(df, aes(x=DNA_Concentration_Ben)) + geom_histogram() + labs (title="DNA Concentration", 
                                                                    subtitle="for Ben", 
                                                                    y="Count", 
                                                                    x="DNA Concentration", 
                                                                    caption="") + theme_minimal()
#Task 2

Year_Collectedfac <- as.factor(df$Year_Collected)
ggplot(df, aes(x=Year_Collectedfac, y=DNA_Concentration_Katy,)) + 
  stat_boxplot(geom='errorbar', linetype=1, width=0.5) + 
  geom_boxplot(outlier.shape=1) + theme_classic()+labs (title="DNA Concentration/Year", 
                                                        subtitle="for Katy", 
                                                        y="DNA Concentration", 
                                                        x="Year Collected", 
                                                        caption="") + theme_classic()  


Year_Collectedfac <- as.factor(df$Year_Collected)
ggplot(df, aes(x=Year_Collectedfac, y=DNA_Concentration_Ben,)) + 
  stat_boxplot(geom='errorbar', linetype=1, width=0.5) + 
  geom_boxplot(outlier.shape=1) + theme_classic() + labs (title="DNA Concentration/Year", 
                                                         subtitle="for Ben", 
                                                         y="DNA Concentration", 
                                                         x="Year Collected", 
                                                         caption="") + theme_classic()



# Get error bars "dashed"

#Task 3
Year_Collectedfac <- as.factor(df$Year_Collected)
ggplot(df, aes(x=Year_Collectedfac, y=DNA_Concentration_Katy,)) + 
  stat_boxplot(geom='errorbar', linetype=1, width=0.5) + 
  geom_boxplot(outlier.shape=1) + theme_classic()+labs (title="DNA Concentration/Year", 
                                                        subtitle="for Katy", 
                                                        y="DNA Concentration", 
                                                        x="Year Collected", 
                                                        caption="") + theme_classic()  
ggsave("Rosati_Plot1.jpeg", width=6)








Year_Collectedfac <- as.factor(df$Year_Collected)
ggplot(df, aes(x=Year_Collectedfac, y=DNA_Concentration_Ben,)) + 
  stat_boxplot(geom='errorbar', linetype=1, width=0.5) + 
  geom_boxplot(outlier.shape=1) + theme_classic() + labs (title="DNA Concentration/Year", 
                                                          subtitle="for Ben", 
                                                          y="DNA Concentration", 
                                                          x="Year Collected", 
                                                          caption="") + theme_classic()
ggsave("Rosati_Plot2.jpeg", width=6)


#Task 4


#Find average of DNA_concentration for years where they are relatively lower


#Ben Average DNA Concentration for each year
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

#Katy Average DNA Concentration for each year
mnk2000 <- with(df, mean(DNA_Concentration_Katy[Year_Collected == 2000])) 
mnk2001 <- with(df, mean(DNA_Concentration_Katy[Year_Collected == 2001]))
mnk2002 <- with(df, mean(DNA_Concentration_Katy[Year_Collected == 2002]))
mnk2003 <- with(df, mean(DNA_Concentration_Katy[Year_Collected == 2003]))
mnk2004 <- with(df, mean(DNA_Concentration_Katy[Year_Collected == 2004]))
mnk2005 <- with(df, mean(DNA_Concentration_Katy[Year_Collected == 2005]))
mnk2006 <- with(df, mean(DNA_Concentration_Katy[Year_Collected == 2006]))
mnk2007 <- with(df, mean(DNA_Concentration_Katy[Year_Collected == 2007]))
mnk2008 <- with(df, mean(DNA_Concentration_Katy[Year_Collected == 2008]))
mnk2010 <- with(df, mean(DNA_Concentration_Katy[Year_Collected == 2010]))
mnk2011 <- with(df, mean(DNA_Concentration_Katy[Year_Collected == 2011]))
mnk2012 <- with(df, mean(DNA_Concentration_Katy[Year_Collected == 2012]))

Year <- c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2010","2011","2012")
Benmean <- c(0.4844,0.5428,0.5651,0.4378,0.7310,1.2838,1.2198,1.4633,1.1849,1.1838,1.1206,1.0921)
Katymean <- c(0.1457,0.0765,0.1208,0.0810,0.1991,0.4923,0.4452,0.556,0.4622,0.4792,0.4487,0.3835)

df2 <- data.frame(Year,Benmean,Katymean)

#Min/Max values for both students
b2000 <- subset(df, Year_Collected ==2000)
b2001 <- subset(df, Year_Collected ==2001)
b2002 <- subset(df, Year_Collected ==2002)
b2003 <- subset(df, Year_Collected ==2003)
b2004 <- subset(df, Year_Collected ==2004)
b2005 <- subset(df, Year_Collected ==2005)
b2006 <- subset(df, Year_Collected ==2006)
b2007 <- subset(df, Year_Collected ==2007)
b2008 <- subset(df, Year_Collected ==2008)
b2010 <- subset(df, Year_Collected ==2010)
b2011 <- subset(df, Year_Collected ==2011)
b2012 <- subset(df, Year_Collected ==2012)

minb2000 <- min(b2000[,"DNA_Concentration_Ben"])
minb2001 <- min(b2001[,"DNA_Concentration_Ben"])
minb2002 <- min(b2002[,"DNA_Concentration_Ben"])
minb2003 <- min(b2003[,"DNA_Concentration_Ben"])
minb2004 <- min(b2004[,"DNA_Concentration_Ben"])
minb2005 <- min(b2005[,"DNA_Concentration_Ben"])
minb2006 <- min(b2006[,"DNA_Concentration_Ben"])
minb2007 <- min(b2007[,"DNA_Concentration_Ben"])
minb2008 <- min(b2008[,"DNA_Concentration_Ben"])
minb2010 <- min(b2010[,"DNA_Concentration_Ben"])
minb2011 <- min(b2011[,"DNA_Concentration_Ben"])
minb2012 <- min(b2012[,"DNA_Concentration_Ben"])

mink2000 <- min(b2000[,"DNA_Concentration_Katy"])
mink2001 <- min(b2001[,"DNA_Concentration_Katy"])
mink2002 <- min(b2002[,"DNA_Concentration_Katy"])
mink2003 <- min(b2003[,"DNA_Concentration_Katy"])
mink2004 <- min(b2004[,"DNA_Concentration_Katy"])
mink2005 <- min(b2005[,"DNA_Concentration_Katy"])
mink2006 <- min(b2006[,"DNA_Concentration_Katy"])
mink2007 <- min(b2007[,"DNA_Concentration_Katy"])
mink2008 <- min(b2008[,"DNA_Concentration_Katy"])
mink2010 <- min(b2010[,"DNA_Concentration_Katy"])
mink2011 <- min(b2011[,"DNA_Concentration_Katy"])
mink2012 <- min(b2012[,"DNA_Concentration_Katy"])


df2$minben <- c(minb2000,minb2001,minb2002,minb2003,minb2004,minb2005,minb2006,minb2007,minb2008,minb2010,minb2011,minb2012)
df2$minkaty <- c(mink2000,mink2001,mink2002,mink2003,mink2004,mink2005,mink2006,mink2007,mink2008,mink2010,mink2011,mink2012)

maxb2000 <- max(b2000[,"DNA_Concentration_Ben"])
maxb2001 <- max(b2001[,"DNA_Concentration_Ben"])
maxb2002 <- max(b2002[,"DNA_Concentration_Ben"])
maxb2003 <- max(b2003[,"DNA_Concentration_Ben"])
maxb2004 <- max(b2004[,"DNA_Concentration_Ben"])
maxb2005 <- max(b2005[,"DNA_Concentration_Ben"])
maxb2006 <- max(b2006[,"DNA_Concentration_Ben"])
maxb2007 <- max(b2007[,"DNA_Concentration_Ben"])
maxb2008 <- max(b2008[,"DNA_Concentration_Ben"])
maxb2010 <- max(b2010[,"DNA_Concentration_Ben"])
maxb2011 <- max(b2011[,"DNA_Concentration_Ben"])
maxb2012 <- max(b2012[,"DNA_Concentration_Ben"])

maxk2000 <- max(b2000[,"DNA_Concentration_Katy"])
maxk2001 <- max(b2001[,"DNA_Concentration_Katy"])
maxk2002 <- max(b2002[,"DNA_Concentration_Katy"])
maxk2003 <- max(b2003[,"DNA_Concentration_Katy"])
maxk2004 <- max(b2004[,"DNA_Concentration_Katy"])
maxk2005 <- max(b2005[,"DNA_Concentration_Katy"])
maxk2006 <- max(b2006[,"DNA_Concentration_Katy"])
maxk2007 <- max(b2007[,"DNA_Concentration_Katy"])
maxk2008 <- max(b2008[,"DNA_Concentration_Katy"])
maxk2010 <- max(b2010[,"DNA_Concentration_Katy"])
maxk2011 <- max(b2011[,"DNA_Concentration_Katy"])
maxk2012 <- max(b2012[,"DNA_Concentration_Katy"])

df2$maxben <- c(maxb2000,maxb2001,maxb2002,maxb2003,maxb2004,maxb2005,maxb2006,maxb2007,maxb2008,maxb2010,maxb2011,maxb2012)
df2$maxkat <- c(maxk2000,maxk2001,maxk2002,maxk2003,maxk2004,maxk2005,maxk2006,maxk2007,maxk2008,maxk2010,maxk2011,maxk2012)


medb2000 <- median(b2000[,"DNA_Concentration_Ben"])
medb2001 <- median(b2001[,"DNA_Concentration_Ben"])
medb2002 <- median(b2002[,"DNA_Concentration_Ben"])
medb2003 <- median(b2003[,"DNA_Concentration_Ben"])
medb2004 <- median(b2004[,"DNA_Concentration_Ben"])
medb2005 <- median(b2005[,"DNA_Concentration_Ben"])
medb2006 <- median(b2006[,"DNA_Concentration_Ben"])
medb2007 <- median(b2007[,"DNA_Concentration_Ben"])
medb2008 <- median(b2008[,"DNA_Concentration_Ben"])
medb2010 <- median(b2010[,"DNA_Concentration_Ben"])
medb2011 <- median(b2011[,"DNA_Concentration_Ben"])
medb2012 <- median(b2012[,"DNA_Concentration_Ben"])


medk2000 <- median(b2000[,"DNA_Concentration_Katy"])
medk2001 <- median(b2001[,"DNA_Concentration_Katy"])
medk2002 <- median(b2002[,"DNA_Concentration_Katy"])
medk2003 <- median(b2003[,"DNA_Concentration_Katy"])
medk2004 <- median(b2004[,"DNA_Concentration_Katy"])
medk2005 <- median(b2005[,"DNA_Concentration_Katy"])
medk2006 <- median(b2006[,"DNA_Concentration_Katy"])
medk2007 <- median(b2007[,"DNA_Concentration_Katy"])
medk2008 <- median(b2008[,"DNA_Concentration_Katy"])
medk2010 <- median(b2010[,"DNA_Concentration_Katy"])
medk2011 <- median(b2011[,"DNA_Concentration_Katy"])
medk2012 <- median(b2012[,"DNA_Concentration_Katy"])

df2$medben <- c(medb2000,medb2001,medb2002,medb2003,medb2004,medb2005,medb2006,medb2007,medb2008,medb2010,medb2011,medb2012)
df2$medkaty <- c(medk2000,medk2001,medk2002,medk2003,medk2004,medk2005,medk2006,medk2007,medk2008,medk2010,medk2011,medk2012)

df2



# in which year is the biggest difference?

#2005

# no graph, min, mean, max, median 

#Task 5
labdata <- df[df$Lab == "Downstairs",]


ggplot(labdata, aes(x=Year_Collected, y=DNA_Concentration_Ben)) + geom_point(shape=1, size=2) + labs (title="DNA Concentration/Year", 
                                                                                                        subtitle="for Ben", 
                                                                                                        y="DNA Concentration", 
                                                                                                        x="Year Collected", 
                                                                                                        caption="") + theme_classic()

#Task6 (bonus)

Benaverage <- data.frame(Year,Benmean)
write.csv(x=Benaverage, file="Ben_Average_Conc.csv")
