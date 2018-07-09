### TDNA line PCA analysis

#load the data as a data frame
Set1 = read.table("TDNA01_final.txt", header=T)

Set1 = data.frame(Set1, stringsAsFactors=TRUE )

#check the spreadsheet
str(Set1)
head(Set1[6:11])

#run PCA
pc<-prcomp(Set1[6:11], scale.=T, retx=T)
pc
plot(pc)
biplot(pc)
names(pc)
summary(pc)
pc$rotation
pc$center

NO3_Level<-Set1$NO3_Level
Genotype<-Set1$Genotype
Experiment<-Set1$Experiment
Plant<-Set1$Plant

pcad=data.frame(pc$x, Plant)
head(pcad)


library(ggplot2)
library(RColorBrewer)

ggplot(data=pcad, aes(x=PC1, y=PC2, colour = NO3_Level, label = NO3_Level)) + geom_point() #+ geom_text()
ggplot(data=pcad, aes(x=PC1, y=PC2, colour = Genotype)) + geom_point() #+ geom_text()


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
     main="Col-O Roots in 1mM and 10mM Nitrate", type="b")

#cumulative scree plot
plot(cumsum(prop_varex), main="Col-O Roots in 1mM and 10mM Nitrate",
     xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", type="b")


#pull out the percentage of contribution (of each parameter) to each PC
rotation = abs(pc$rotation)
sweep(rotation, 2, colSums(rotation), "/")

merged =merge(Set1, pcad, by.x = "Plant", by.y = "Plant")
head(merged)
write.csv(merged, "~/Desktop/RootPhenotyping/Final_Combined_Analysis/PCAsforANOVA/TDNA_45_PCA.csv")

ANOVAData <- read.csv("~/Desktop/RootPhenotyping/Final_Combined_Analysis/PCAsforANOVA/TDNA_45_PCA.csv", header=T)
head(ANOVAData)


anova_PCA1 = lm(PC1 ~ Genotype*NO3_Level + Plate, data = ANOVAData)
anova(anova_PCA1)

anova_PCA2 = lm(PC2 ~ Genotype*NO3_Level + Plate, data = ANOVAData)
anova(anova_PCA2)

anova_PCA3 = lm(PC3 ~ Genotype*NO3_Level + Plate, data = ANOVAData)
anova(anova_PCA3)

anova_PCA4 = lm(PC4 ~ Genotype*NO3_Level + Plate, data = ANOVAData)
anova(anova_PCA4)

anova_PCA5 = lm(PC5 ~ Genotype*NO3_Level, data = ANOVAData)
anova(anova_PCA5)

