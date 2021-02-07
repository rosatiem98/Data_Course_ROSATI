df1 <- read.csv("/Documents and Settings/emros/Desktop/Data_Course_ROSATI/Data/ITS_mapping.csv")

library(tidyverse)
df2 <- data.frame(do.call("rbind", strsplit(as.character(df1$SampleID.BarcodeSequence.LinkerPrimerSequence.Run.Ecosystem.Island.Lat.Lon.Collection_Date.F_Primer.R_Primer.Ecosys_Type.Host_Type.Host.InputFileName.Description), " ", fixed = TRUE)))
df3 <- data.frame(do.call("rbind", strsplit(as.vector(df1$SampleID.BarcodeSequence.LinkerPrimerSequence.Run.Ecosystem.Island.Lat.Lon.Collection_Date.F_Primer.R_Primer.Ecosys_Type.Host_Type.Host.InputFileName.Description), "_", fixed = TRUE)))
df8 <- unique(df5)
df8$X8 <- NULL

library(dplyr)

library("stringr")
df9 <- str_split_fixed(df8$X1, " and ", 2)
nums <- unlist(lapply(df, is.numeric))
nums2 <- filter(is.numeric, df)
