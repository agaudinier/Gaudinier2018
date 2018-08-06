# This code will analyze root phenotypes using an ANOVA
# to look at genotype, nitrate level, genotype*nitrate level, plate
# to determine the contribution each of these factors
# has on the root system phenotypes measured

# PR = primary root length (cm)
# LR = lateral root number (visible from scan)
# LRL = total lateral root lenth (all LRs added together - cm)
# ALRL = average lateral root length (LRL/LR - cm)
# TRL = total root length (PR + LRL)
# LRD = lateral root density (LR/PR)
# LRL_div_TRL = percentage of LRL contributing to TRL (LRL/TRL)


#load the data as a data frame
Set1 <- read.table("TDNA_01.txt", header=T)

#make the dataset a dataframe
Roots1 <- data.frame(Set1, stringsAsFactors=TRUE )

#check the dataframe
head(Roots1)

#ensure that nitrate level is considered a factor
Roots1$NO3_Level <- factor(Roots1$NO3_Level)

#check structure of the dataframe
str(Roots1)

#use lsmeans
library(lsmeans)

#linear model for primary root (PR) measurements
anova_PR <- lm(logPR ~ Genotype*NO3_Level + Plate, data = Roots1)

#plot data to ensure it looks normal
plot(anova_PR)

#ANOVA of the linear model
m1<-anova(anova_PR)
m1

#look at constrasts for genotype*nitrate level interaction
PR_interaction_lsmeans <- lsmeans(anova_PR, pairwise ~ Genotype*NO3_Level)
PR_interaction_lsmeans[[2]]

#plot data to see if mutant roots are larger or smaller than Col-0 control
boxplot (logPR ~ Genotype*NO3_Level, data=Roots1)


# Number of lateral roots (LR)
anova_LR <- lm(logLR ~ Genotype*NO3_Level + Plate, data = Roots1)
plot(anova_LR)
m2<-anova(anova_LR)
m2
LR_interaction_lsmeans <- lsmeans(anova_LR, pairwise ~ Genotype*NO3_Level)
LR_interaction_lsmeans[[2]]

boxplot (logLR ~ Genotype*NO3_Level, data=Roots1)


# test if the LR relies on PR as a factor
anovaPR_LR <- lm(logLR ~ Genotype*NO3_Level + Plate + logPR, data = Roots1)
plot(anovaPR_LR)
m2_1<-anova(anovaPR_LR)
m2_1
LR_interaction_lsmeans_PR <- lsmeans(anovaPR_LR, pairwise ~ Genotype*NO3_Level)
LR_interaction_lsmeans_PR[[2]]


# Total lateral root length (LRL)
anova_LRL <- lm(logLRL ~ Genotype*NO3_Level + Plate, data = Roots1)
plot(anova_LRL)
m3<-anova(anova_LRL)
m3
LRL_interaction_lsmeans <- lsmeans(anova_LRL, pairwise ~ Genotype*NO3_Level)
LRL_interaction_lsmeans[[2]]

boxplot (logLRL ~ Genotype*NO3_Level, data=Roots1)


# test if the LRL relies on PR as a factor
anovaPR_LRL <- lm(logLRL ~ Genotype*NO3_Level + Plate + logPR, data = Roots1)
plot(anovaPR_LRL)
m3_1<-anova(anovaPR_LRL)
m3_1
LRL_interaction_lsmeans_PR <- lsmeans(anovaPR_LRL, pairwise ~ Genotype*NO3_Level)
LRL_interaction_lsmeans_PR[[2]]


# average lateral root length (ALRL) (LRL/LR)
anova_ALRL <- lm(logALRL ~  Genotype*NO3_Level + Plate, data = Roots1)
plot(anova_ALRL)
m5<-anova(anova_ALRL)
m5
ALRL_interaction_lsmeans <- lsmeans(anova_ALRL, pairwise ~ Genotype*NO3_Level)
ALRL_interaction_lsmeans[[2]]

boxplot (logALRL ~ Genotype*NO3_Level, data=Roots1)


# test if the ALRL relies on PR as a factor
anovaPR_ALRL <- lm(logALRL ~  Genotype*NO3_Level + Plate + logPR, data = Roots1)
plot(anovaPR_ALRL)
m5_1<-anova(anovaPR_ALRL)
m5_1
ALRL_interaction_lsmeans_PR <- lsmeans(anovaPR_ALRL, pairwise ~ Genotype*NO3_Level)
ALRL_interaction_lsmeans_PR[[2]]


# total root length (TRL) (PR + LRL)
anova_TRL <- lm(logTRL ~ Genotype*NO3_Level + Plate, data = Roots1)
plot(anova_TRL)
m4<-anova(anova_TRL)
m4
TRL_interaction_lsmeans <- lsmeans(anova_TRL, pairwise ~ Genotype*NO3_Level)
TRL_interaction_lsmeans[[2]]

boxplot (logTRL ~ Genotype*NO3_Level, data=Roots1)


# lateral root density (LRD) (LR/PR)
anova_LRD <- lm(logLRD ~ Genotype*NO3_Level + Plate, data = Roots1)
plot(anova_LRD)
m6<-anova(anova_LRD)
m6
LRD_interaction_lsmeans <- lsmeans(anova_LRD, pairwise ~ Genotype*NO3_Level)
LRD_interaction_lsmeans[[2]]

boxplot (LRD ~ Genotype*NO3_Level, data=Roots1)


# percentage of LRL contributing to TRL (LRL/TRL)
anova_LRL_div_TRL <- lm(logLRL_div_TRL ~ Genotype*NO3_Level + Plate, data = Roots1)
plot(anova_LRL_div_TRL)
m7<-anova(anova_LRL_div_TRL)
m7
LRL_div_TRL_interaction_lsmeans <- lsmeans(anova_LRL_div_TRL, pairwise ~ Genotype*NO3_Level)
LRL_div_TRL_interaction_lsmeans[[2]]

boxplot (logLRL_div_TRL ~ Genotype*NO3_Level, data=Roots1)
