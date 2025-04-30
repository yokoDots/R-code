# libraries ####
library(bugsigdbr)
library(BugSigDBStats)
library(ComplexHeatmap)
library(ggpubr)
library(tidyverse)
setwd("~/Documents/statistics/Rsito/bugsigdb")

#import ####
full.dat <- importBugSigDB(cache = FALSE)

# get signatures per study ####
## study and signature dataframe
study.taxa <- cbind(full.dat$Study, 
                    full.dat$`MetaPhlAn taxon names` )
study.taxa <- data.frame(study.taxa, row.names = NULL)
names(study.taxa) <- c("Study", "Taxa")
# signatures is in list form per experiment
study.taxa <- unnest(study.taxa, Taxa)
which(is.na(study.taxa))
study.taxa <- na.omit(study.taxa) # remove nas, 27003 rows
study.taxa <- as.data.frame(study.taxa) # was having issues
study.taxa$Study <- as.character(study.taxa$Study) #with Study not being recognized

#signatures per study
table(study.taxa$Study)

# get unknown taxa ####
unknownsb <- grepl(" bacterium", 
                   study.taxa[["Taxa"]])
unknownsu <- grepl("uncultured", study.taxa[["Taxa"]])
unknownssp <- grepl(" sp", study.taxa[["Taxa"]])

unknowns <- data.frame(unknownsb, unknownsu, unknownssp)
unknowns <- unknowns %>% mutate(unknowns = 
                                  as.logical(
                                    unknownsb + unknownsu + unknownssp))

study.taxa$Uncertain <- unknowns$unknowns

## add 0's to study to keep order ####
x <- study.taxa$Study
study.taxa$Study <- sprintf("%03d%s", 
                            as.numeric(gsub("[^0-9]+", "", x)), 
                            gsub("[0-9]+", "", x))

# cumulative frequencies ####
# objective: graph increases or not of studies vs signatures

taxsum <- as.data.frame(table(study.taxa$Study),
                        row.names = NULL)
taxsum$cs <- cumsum(taxsum$Freq)

uncertains <- data.frame(table(study.taxa$Study, 
                               study.taxa$Uncertain))
uncertains <- subset(uncertains, Var2 == "TRUE")
taxsum$csu <- cumsum(uncertains$Freq)

taxsum$num <- c(1:length(taxsum[,1]))
taxsum$pct_uncertain <- round((taxsum$csu / taxsum$cs) * 100, 2)
