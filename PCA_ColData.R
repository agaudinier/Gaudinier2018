#PCA for Col-0 Root traits

#upload data
ColData <- read.csv("~/Desktop/RootPhenotyping/COL_ROOTS4.csv", header=T)

ColData <- data.frame(ColData, stringsAsFactors=TRUE )

#check the spreadsheet for type of data and that it was read in correctly
str(ColData)
head(ColData)

#make sure that NO3_Level and Set# are factors
ColData$NO3_Level = factor(ColData$NO3_Level)
ColData$Set = factor(ColData$Set)

#run PCA of traits PR, LR, LRL, TRL, ALRL, LRD, LRLdivTRL
pc<-prcomp(ColData[4:9], scale.=T, retx=T)
pc
plot(pc)
biplot(pc)
names(pc)
summary(pc)
pc$rotation
pc$center

NO3_Level<-ColData$NO3_Level
vars

RootSet<-ColData$Set
vars2

Plant = ColData$Plant


pcad=data.frame(pc$x, NO3_Level)
head(pcad)
pcad2=data.frame(pc$x, RootSet)
head(pcad2)
pcad3 = data.frame(pc$x, Plant)
pcad3

library(ggplot2)
library(RColorBrewer)


ggplot(data=pcad, aes(x=PC1, y=PC2, colour = NO3_Level, label = Plant)) + geom_point() + geom_text()
ggplot(data=pcad, aes(x=PC1, y=PC2, colour = NO3_Level)) + geom_point() #+ geom_text()
nitrate



ggplot(data=pcad2, aes(x=PC1, y=PC2, colour = RootSet)) + geom_point()
setplot

ggsave("~/Documents/Research/R_Code/PCA/devggPC1.2.pdf")

ggplot(data=pcad, aes(x=PC2, y=PC3, colour=RootSet)) + geom_point() +  theme_bw()

ggsave("~/Documents/Research/R_Code/PCA/devggPC2.3.pdf")

ggplot(data=pcad, aes(x=PC3, y=PC4, colour=RootSet)) + geom_point()

ggsave("~/Documents/Research/R_Code/PCA/devggPC3.4.pdf")

ggplot(data=pcad, aes(x=PC4, y=PC5, colour=RootSet)) + geom_point()

ggsave("~/Documents/Research/R_Code/PCA/devggPC4.5.pdf")

#compute SD of each principal component
std_dev<-pc$sdev

#compute variance
pr_var<-std_dev^2
pr_var[1:5] #check var of first 5 components

#proportion of variance explained
prop_varex<-pr_var/sum(pr_var)
prop_varex[1:5]

sum(prop_varex[1:5]) #first 5 component explain 100% of variance

#scree plot to access components or factors explaining most of variability in data
plot(prop_varex, xlab="Principal Component", ylab="Proportion of Variance Explained", 
     main="Col-O Roots vs TDNA03", type="b")

#cumulative scree plot
plot(cumsum(prop_varex), main="Col-O Roots vs TDNA03",
     xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", type="b")


#pull out the percentage of contribution (of each parameter) to each PC
rotation = abs(pc$rotation)
sweep(rotation, 2, colSums(rotation), "/")


merged =merge(Set1, pcad, by.x = "Plant", by.y = "Plant")
head(merged)
write.csv(merged, "~/Desktop/RootPhenotyping/Final_Combined_Analysis/PCAsforANOVA/TDNA_03_PCA.csv")

ANOVAData <- read.csv("~/Desktop/RootPhenotyping/Final_Combined_Analysis/PCAsforANOVA/TDNA_03_PCA.csv", header=T)
head(ANOVAData)

library(afex)

anova_PCA1 = lm(PC1 ~ Genotype*NO3_Level + Plate, data = ANOVAData)
anova(anova_PCA1)

anova_PCA2 = lm(PC2 ~ Genotype*NO3_Level + Plate, data = ANOVAData)
anova(anova_PCA2)

anova_PCA3 = lm(PC3 ~ Genotype*NO3_Level, data = ANOVAData)
anova(anova_PCA3)

anova_PCA4 = lm(PC4 ~ Genotype*NO3_Level, data = ANOVAData)
anova(anova_PCA4)

anova_PCA5 = lm(PC5 ~ Genotype*NO3_Level, data = ANOVAData)
anova(anova_PCA5)



##########
