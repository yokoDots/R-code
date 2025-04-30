setwd("~/Documents/statistics/Rsito/bugsigdb")
library(bugsigdbr)
library(BugSigDBStats)
library(tidyverse)
# import ####
bsdb <- importBugSigDB(cache = FALSE)
# Get studies that have fungi ####
fungi<-grep("k__Fungi", #returns true/false matrix
             bsdb[["MetaPhlAn taxon names"]])
fungi<-bsdb[fungi,] # 64 signatures that have fungi taxa
unique(fungi$Study) #27 studies
unique(bsdb$Study) # 849 studies
# another way of just fungi
# fungi<-bsdb[grep("k__Fungi", 
#       bsdb$`MetaPhlAn taxon names`), ] #64

# all signatures (even with no fungi) in studies with fungi
# filter all rows matching fungi$Study in bsdb
exps.fungi<-bsdb %>%   #filter by study
  filter_all(any_vars(. %in% fungi$Study)) #157
#other ways of getting all signatures in studies with fungi
#fungi<-subset(bsdb, Study %in% unique(fungi$Study)) #157
#fungi<-bsdb %>%filter(Study %in% fungi$Study)#157

## count studies ####
fungi %>% count(Study) #27
unique(fungi$Study)  # 27
table(fungi$Study) # same as count

## signatures with fungi ####
sigs <- getSignatures(fungi, tax.id.type = "metaphlan") #list 64

## Number of taxa ####
 sum(lengths(sigs)) #455 
 length(unlist(sigs)) # total signatures 455 
 length(unique(unlist(sigs))) # unique signatures 333
 
 sigName <- getSignatures(fungi,
            tax.id.type = "taxname")
 length((unlist(sigName)))# 455
 table(unlist(sigName))
# full db 
allName <- getSignatures(bsdb,
          tax.id.type = "taxname")
length(unlist(allName)) #26840

## fungi taxa only ####
onlyFungi<-grep("k__Fungi", 
           unlist(fungi$`MetaPhlAn taxon names`))
onlyFungi<-unlist(fungi$`MetaPhlAn taxon names`)[onlyFungi] #160

# no repeat
unique(unlist(onlyFungi)) #105

### extract rank name ####
onlyFungi<- lapply(onlyFungi, \(x) sub(".*\\|", "", x))
onlyFungi<-lapply(onlyFungi, \(x) substr(x, 4, nchar(x)))

# Fungi taxa vs bacterial taxa ####
# studies that have fungi
library(RColorBrewer)
clr1<- brewer.pal(3, "Pastel1")
unique(bsdb$Study) #849
unique(fungi$Study) # 27
barplot(c(849,27), names=c("All studies", "Studies with fungi"),
        col = clr1, border = FALSE,
        ylab="Studies",
        space = 0.2, pos = -0.1,
        main="BugSigDB studies with fungi taxa",
        cex.axis=0.6, cex.lab=0.7,
        cex.names = 0.7, cex.main =0.7,
        ylim = c(0,1000))

## get all taxa
uAllSig<-length(unique(unlist(allName))) # all unique taxa 2951
#all taxa in signatures with fungi
uFuSig<-length(unique(unlist(sigs))) # 333
#unique fungi signatures
uOFSig <- length(unique(unlist(onlyFungi))) #105

## Fungi taxa in signatures
library(RColorBrewer)
clr2<- brewer.pal(3, "Pastel2") 
barplot(c(uAllSig,uFuSig,uOFSig), 
        names=c("All signatures", "Fungi Signatures", 
        "Fungi taxa"),
        border=clr2, col = clr2,
        ylab="Number of taxa", 
        main="Number of taxa in signatures",
        space = 0.4, pos = -0.1,
        cex.axis=0.6, cex.lab=0.7,
        cex.names = 0.7, cex.main =0.7,
        ylim = c(0,3000))

## Highest frequencies ####
fungi.table<-table(unlist(onlyFungi))
fdf <- data.frame(taxon = names(fungi.table), 
            number = as.integer(fungi.table))
f.max<-slice_max(fdf, order_by = number, n=20)

### top 20 fungi taxa ####
ggplot(f.max, aes(taxon, number))+
  geom_col(fill="#89a5c3")+
  theme_classic()+
  ylab("Frequency") + xlab("Top 20 Fungi taxa")+
  theme(text = element_text(size = 9),
      axis.text.x = element_text(angle = 90,
      size = 7.5, vjust = 0.3, hjust=1))

all.table<-table(unlist(sigName))

adf <- data.frame(taxon = names(all.table), 
                  number = as.integer(all.table))
# add Fungi/Other kingdom
adf2<-left_join(adf,fdf, by = "taxon")%>%
  mutate(number.y = if_else(is.na(number.y),
                "Other", "Fungi"))
names(adf2)<-c("Taxon", "Number", "Kingdom")
a.max<-slice_max(adf2, order_by = Number, n=20)

### top 20 all taxa ####
ggplot(a.max, aes(Taxon, Number,fill=Kingdom))+
  geom_col()+
  theme_classic()+
  ylab("Frequency") + xlab("Top 20 all taxa")+
  theme(text = element_text(size = 9),
        axis.text.x = element_text(angle = 90,
        size = 7.5, vjust = 0.3, hjust=1))

# UP/DOWN #### 
# taxon listed by experiment
up.down <- stack(sapply(unlist(sigName, 
        recursive = FALSE), toString))[2:1]
up.down$ind<-substr(up.down$ind, 0,12)
up.down$ind<-sub("_", "", up.down$ind)

# up down sigs
up.down<- up.down %>%
  mutate(ind = if_else(
    str_extract(ind, "(?<=\\/)\\d+$")=="1",
    "Increase", "Decrease")) %>%
  rename("Direction"=ind, "Taxon"=values)

with(up.down, table(Direction,Taxon))
# top 10 increase or decrease
up.down.20 <- up.down %>%
  group_by(Direction, Taxon) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(Direction) %>%                       
  slice_max(order_by = Count, n = 10)

ggplot(up.down.20,
  aes(x=Taxon, y = ifelse(Direction == "Increase", 
  Count, -Count)))+ 
  geom_bar(stat="identity", position="identity", 
  aes(fill=Direction)) +
  ylab("Direction frequency")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, 
        hjust = 1, vjust = 0.5, size = 8),
        legend.key.size = unit(0.4, "cm"))+
        coord_flip()

# Body sites ####
## body sites frequency ####
Body<-table(fungi$`Body site`)
Body <- data.frame(Site = names(Body), 
      Experiments = as.integer(Body))
Body<-Body[order(decreasing = T, Body$Experiments),]
clipr::write_clip(Body, object_type = "table")

# taxa per body site
#use cbind because Metaphlan taxon is a list type
AllBody <- cbind(fungi$`Body site`, 
        fungi$`MetaPhlAn taxon names` )
AllBody<-as.data.frame(AllBody, row.names = NULL)
names(AllBody)<-c("Body", "Taxa")
## All taxa ####
AllBody<-AllBody%>%unnest('Taxa') #455
## Only fungi ####
fungiBody<-AllBody[grep("k__Fungi", AllBody$Taxa), ] #160

# clean
AllBody<- lapply(AllBody,\(x) sub(".*\\|", "", x))
AllBody<-lapply(AllBody,\(x) sub("\")", "", x))
AllBody$Taxa<-gsub("^.{0,3}", "", AllBody$Taxa)

fungiBody<- lapply(fungiBody,\(x) sub(".*\\|", "", x))
fungiBody<-lapply(fungiBody,\(x) sub("\")", "", x))
fungiBody$Taxa<-gsub("^.{0,3}", "", fungiBody$Taxa)

## Frequency #### per body site
freqAB<-data.frame(table(AllBody$Body,AllBody$Taxa))
freqAB<-subset(freqAB, Freq>1) %>%
  rename("Body_Site"=Var1, "Taxon"=Var2)

ggplot(freqAB, aes(x=Body_Site,Taxon)) + 
  geom_tile(aes(fill=Freq)) +
  theme_classic()+
  theme(text = element_text(size = 8),
        axis.text.x = element_text(angle = 90,
        size = 7.5, vjust = 0.3, hjust=1))

freqFB<-data.frame(table(fungiBody$Body, fungiBody$Taxa))
freqFB<-subset(freqFB, Freq>1) %>%
  rename("Body_Site"=Var1, "Taxon"=Var2)

ggplot(freqFB, aes(x=Body_Site,Taxon)) + 
  geom_tile(aes(fill=Freq)) +
  theme_classic()+
  theme(text = element_text(size = 8),
        axis.text.x = element_text(angle = 90,
          size = 7.5, vjust = 0.3, hjust=1))

# Conditions ####
conditionsAll <-unique(bsdb$Condition) # 333
conditionFungi <-unique(fungi$Condition) #25
clipr::write_clip(conditionFungi, object_type = "table")



# TO DO ####
## get a list of sites where c albicans shows up
## see studies with c.albicans and bacteroidetes and firmicutes
## how many studies have Malassezia restricta
## Histoplasma, Coccidiodes, Paracoccidioides, Blastomyces, and Aspergillus,
## Cryptococcus neoformans, Cryptococcus
