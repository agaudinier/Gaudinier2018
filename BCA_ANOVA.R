#Code for Total Protein (BCA) analysis

#Load data
Set1 <- read.csv("~/Desktop/WholePlantPhenotyping/BCA/allgenotypes_BCA_8_3_17.csv", header=T)
Set2 <- read.csv("~/Desktop/WholePlantPhenotyping/BCA/46and47_BCA_8_3_17.csv", header=T)


Prot <- data.frame(Set1, stringsAsFactors=TRUE )
Prot2 <- data.frame(Set2, stringsAsFactors=TRUE )

#this will order the genotypes from lowest to highest protein content for set1
geno <- c("arf18-3","hmgb15-1", "gnc" ,"myb29-1", "erf107-1", "bbx16-1",  "hat22-1", 
          "anac032-1", "arf18-2", "arid5-1", "nac102-1", "rav2-1", "Col-0","lbd4-1", "arf9-2")
Prot$Genotype <- factor(Prot$Genotype, levels = geno)

#visualize the data
boxplot(average ~ Genotype, data = Prot)

#these genotypes were analyzed in a separate experiment (set2)
#this orders these genotypes from lowest to highest protein content
geno2 <- c("nlp7-1","chl1-5", "Col-0")
Prot2$Genotype <- factor(Prot$Genotype, levels = geno2)
#visualize the data
boxplot(average ~ Genotype, data = Prot2)

##ANOVAs

#subset data for WT comparision
subset1 <- Prot %>%
  filter(Genotype %in% c("Col-0", "arf18-3"))

#run anova to look for Genotype effects in differences in protein content
#protein extraction plate is an additional factor
anova_Prot1 = lm(Protein_Content ~ Genotype + plate, data = subset1)
m1=anova(anova_Prot1)
m1

###
subset2 <- Prot %>%
  filter(Genotype %in% c("Col-0", "hmgb15-1"))

#run anova to look for Genotype effects in differences in protein content
#protein extraction plate is an additional factor
anova_Prot2 = lm(Protein_Content ~ Genotype + plate, data = subset2)
m2=anova(anova_Prot2)
m2

###
subset3 <- Prot %>%
  filter(Genotype %in% c("Col-0", "gnc"))

#run anova to look for Genotype effects in differences in protein content
#protein extraction plate is an additional factor
anova_Prot3 = lm(Protein_Content ~ Genotype + plate, data = subset3)
m3=anova(anova_Prot3)
m3

###
subset4 <- Prot %>%
  filter(Genotype %in% c("Col-0", "myb29-1"))

#run anova to look for Genotype effects in differences in protein content
#protein extraction plate is an additional factor
anova_Prot4 = lm(Protein_Content ~ Genotype + plate, data = subset4)
m4=anova(anova_Prot4)
m4

###
subset5 <- Prot %>%
  filter(Genotype %in% c("Col-0", "erf107-1"))

#run anova to look for Genotype effects in differences in protein content
#protein extraction plate is an additional factor
anova_Prot5 = lm(Protein_Content ~ Genotype + plate, data = subset5)
m5=anova(anova_Prot5)
m5

###
subset6 <- Prot %>%
  filter(Genotype %in% c("Col-0", "bbx16-1"))

#run anova to look for Genotype effects in differences in protein content
#protein extraction plate is an additional factor
anova_Prot6 = lm(Protein_Content ~ Genotype + plate, data = subset6)
m6=anova(anova_Prot6)
m6

###
subset7 <- Prot %>%
  filter(Genotype %in% c("Col-0", "hat22-1"))

#run anova to look for Genotype effects in differences in protein content
#protein extraction plate is an additional factor
anova_Prot7 = lm(Protein_Content ~ Genotype + plate, data = subset7)
m7=anova(anova_Prot7)
m7

###
subset8 <- Prot %>%
  filter(Genotype %in% c("Col-0", "anac032-1"))

#run anova to look for Genotype effects in differences in protein content
#protein extraction plate is an additional factor
anova_Prot8 = lm(Protein_Content ~ Genotype + plate, data = subset8)
m8=anova(anova_Prot8)
m8

###
subset9 <- Prot %>%
  filter(Genotype %in% c("Col-0", "arf18-2"))

#run anova to look for Genotype effects in differences in protein content
#protein extraction plate is an additional factor
anova_Prot9 = lm(Protein_Content ~ Genotype + plate, data = subset9)
m9=anova(anova_Prot9)
m9

###
subset10 <- Prot %>%
  filter(Genotype %in% c("Col-0", "arid5-1"))

#run anova to look for Genotype effects in differences in protein content
#protein extraction plate is an additional factor
anova_Prot10 = lm(Protein_Content ~ Genotype + plate, data = subset10)
m10=anova(anova_Prot10)
m10

###
subset11 <- Prot %>%
  filter(Genotype %in% c("Col-0", "nac102-1"))

#run anova to look for Genotype effects in differences in protein content
#protein extraction plate is an additional factor
anova_Prot11 = lm(Protein_Content ~ Genotype + plate, data = subset11)
m11=anova(anova_Prot11)
m11

###
subset12 <- Prot %>%
  filter(Genotype %in% c("Col-0", "rav2-1"))

#run anova to look for Genotype effects in differences in protein content
#protein extraction plate is an additional factor
anova_Prot12 = lm(Protein_Content ~ Genotype + plate, data = subset12)
m12=anova(anova_Prot12)
m12

###
subset13 <- Prot %>%
  filter(Genotype %in% c("Col-0", "lbd4-1"))

#run anova to look for Genotype effects in differences in protein content
#protein extraction plate is an additional factor
anova_Prot13 = lm(Protein_Content ~ Genotype + plate, data = subset13)
m13=anova(anova_Prot13)
m13

###
subset14 <- Prot %>%
  filter(Genotype %in% c("Col-0", "arf9-2"))

#run anova to look for Genotype effects in differences in protein content
#protein extraction plate is an additional factor
anova_Prot14 = lm(Protein_Content ~ Genotype + plate, data = subset13)
m14=anova(anova_Prot14)
m14

###
subset15 <- Prot2 %>%
  filter(Genotype %in% c("Col-0", "chl1-5"))

#run anova to look for Genotype effects in differences in protein content
#protein extraction plate is an additional factor
anova_Prot15 = lm(Protein_Content ~ Genotype + plate, data = subset15)
m15=anova(anova_Prot15)
m15


###
subset16 <- Prot2 %>%
  filter(Genotype %in% c("Col-0", "nlp7-1"))

#run anova to look for Genotype effects in differences in protein content
#protein extraction plate is an additional factor
anova_Prot16 = lm(Protein_Content ~ Genotype + plate, data = subset16)
m16=anova(anova_Prot16)
m16
