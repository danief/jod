#---------------------------------------------------------------------
#
# Forberedelse av kostholdsundersokelsesdata  
# for testkjøring av MCRA
# 
# Daniel Flo, 21.06.2018, VKM
#
#---------------------------------------------------------------------
# last inn pakker
library(reshape2)
library(tidyverse)
#---------------------------------------------------------------------
# les data
setwd("C:/Users/DAFL/Desktop/Jod/Data2")
list.files()
c<-read.csv("Consumptions1.csv", header = T, sep = ";", dec=".")
head(c[,1:3])
dim(c) # 1787 observasioner fordelt paa  1118 variabler
#---------------------------------------------------------------------
# bytt komma med dot
c<-as.data.frame(sapply(c, gsub, pattern = ",", replacement= "."))
head(c)
str(c)
#---------------------------------------------------------------------
# fra lang til kort datastruktur  
t<- melt(c, id.vars=c("idFood", "IdInduvidual"))
head(t, 50)
dim(t) # 1994292 observasioner fordelt paa  4 variabler
t$Amount<-as.numeric(t$Amount)
#---------------------------------------------------------------------
# legg til kolonne med benevning i mg?
t$idUnit <- rep("mg", length(t))
head(t, 50)
# forandre kolonnenavn til MCRA standard navn 
names(t)<-c("idFood" ,"IdInduvidual" ,"variable", "Amount" ,"idUnit")
# dropp  kolonne med navn "variable"
t <- t[ -c(3) ]
str(t)
# sett verdien null til NA og fjern 
t[t == 0] <- NA
t<-na.omit(t)
t$Amount<-as.numeric(t$Amount)
# set kolonner til "Character vectors" text
t$IdInduvidual<-as.character(t$IdInduvidual)
t$idFood<-as.character(t$idFood)
# sjekk
head(t)
str(t)
dim(t) # 64173 observasioner fordelt paa  4 variabler
#---------------------------------------------------------------------
# skriv ut .csv fil til opplasting i MCRA

# write.csv(t, "Consumptions1_2.csv", sep = ",", quote=F)

#---------------------------------------------------------------------
# plott og sjekk data

# histogram 
ggplot(data = t, aes(Amount, fill = idFood)) +
  geom_histogram(binwidth = 500) +
  theme(legend.position = "none")

# bobleplot
ggplot(t, aes(x = idFood, y = Amount)) +
  geom_point(aes(size = Amount, colour = Amount)) + 
  scale_colour_gradient(low = "blue", high = "red") +
  scale_y_sqrt() +
  scale_size(range = c(1, 15)) +
  theme_bw() +
  theme(legend.position = "none")
#---------------------------------------------------------------------
# stopp