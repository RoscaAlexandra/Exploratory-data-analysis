data(VADeaths)
install.packages("gridExtra")
install.packages("ggplot2")
install.packages("vcd")
install.packages("vcdExtra")
install.packages("seriation")
install.packages("Select")
install.packages('purrr')
library("gridExtra")
library("ggplot2")
library("vcd")
library("vcdExtra")

##########
#univariables
require(lattice) 

VADeathRate <- as.data.frame.table(VADeaths)
#VADeathRate$Var1 <- sum(VADeathRate$Var1, VADeathRate$Var2)
#aggregate(VADeathRate$points, by=list(Players=x$Players), FUN=mean)

ageGrouping <- aggregate(VADeathRate$Freq, by=list(Age=VADeathRate$Var1), FUN=mean)
catGrouping <- aggregate(VADeathRate$Freq, by=list(Category=VADeathRate$Var2), FUN=mean)
type1Grouping <- aggregate(VADeathRate$Freq, by=list(gender=VADeathRate$Var2), FUN=mean)
type1Grouping$Place <- c("Rural", "Rural", "Urban", "Urban")

placeGrouping <- aggregate(type1Grouping$x, by=list(Place=type1Grouping$Place), FUN=mean)

hist1 <- ggplot(placeGrouping, aes(Place, x, fill=Place, width=x*0.01)) +
  
  geom_bar(stat="identity") +
  ylab("Death rate")

hist2 <- ggplot(ageGrouping, aes(Age, x, fill = Age)) +
  
  geom_bar(stat="identity") +
  ylab("Death rate")



hist3 <- ggplot(catGrouping, aes(Category, x, fill=Category, width=x*0.02)) +
  geom_bar(stat="identity") +
  geom_col(position = "dodge") +
  ylab("Death rate")


grid.arrange(hist1, hist2, hist3)


####
#multivariables
#library(help = "datasets")
#UCBAdmissions
uniAdmission <- as.data.frame.table(UCBAdmissions)

#Admission depending on gender
doubledecker(Admit ~ Gender, data = UCBAdmissions,
             gp = gpar(fill = c("#0caa00","#c70b00")))
#Admission depending on dept
doubledecker(Admit ~ Dept, data = UCBAdmissions,
             gp = gpar(fill = c("#0caa00","#c70b00")))
#Admission depending on dept and gender
doubledecker(Admit ~ Dept + Gender, data = UCBAdmissions, gp = gpar(fill = c("#0caa00","#c70b00")))

#Death rates mosaics
addedCols <- VADeathRate
addedCols$Place <- c("Rural", "Rural", "Rural", "Rural", "Rural", "Rural", "Rural", "Rural", "Rural", "Rural", "Urban", "Urban", "Urban", "Urban", "Urban", "Urban", "Urban", "Urban", "Urban", "Urban")
addedCols$Sex <- c("Male", "Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female", "Female", "Male", "Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female", "Female")
par(mfrow=c(1,3))
m1 <- mosaicplot(xtabs(Freq ~ Sex, data=addedCols), main="", color = "#0c9cac", border = "black")
m2 <- mosaicplot(xtabs(Freq ~ Sex + Var1, data=addedCols),main="", color = "#0c9cac", border = "black")
m3 <- mosaicplot(xtabs(Freq ~ Sex + Var1 + Place, data=addedCols),main="", color = "#0c9cac", border = "black")



library("purrr")
library(dplyr)

mosaic(table(select(Zoo, c(hair, eggs:backbone))))
