##Elemental Analysis

##read in data
Set1 = read_csv("~/Desktop/WholePlantPhenotyping/ElementalAnalysis/ElementalAnalysis.csv")

#check dataset
head(Set1)

library(tidyverse)

#look at boxplots of all data
boxplot(C_per_ug ~ Genotype, data = Set1)

boxplot(N_per_ug ~ Genotype, data = Set1)

boxplot(C_to_N ~ Genotype, data = Set1)

##filter data for Col-0 vs T-DNA ANOVAs

#### nac032-1
nac032 <- Set1 %>% 
  filter(Genotype %in% c("Col-0", "nac032-1"))

C_lm <- stats::lm(C_per_ug ~ Genotype , data = nac032)
summary(aov(C_lm))

N_lm <- stats::lm(N_per_ug ~ Genotype , data = nac032)
summary(aov(N_lm))

CN_lm <- stats::lm(C_to_N ~ Genotype , data = nac032)
summary(aov(CN_lm))

### arf18-2
arf182 <- Set1 %>% 
  filter(Genotype %in% c("Col-0", "arf18-2"))

C_lm <- stats::lm(C_per_ug ~ Genotype , data = arf182)
summary(aov(C_lm))

N_lm <- stats::lm(N_per_ug ~ Genotype , data = arf182)
summary(aov(N_lm))

CN_lm <- stats::lm(C_to_N ~ Genotype , data = arf182)
summary(aov(CN_lm))

### arf18-3
arf183 <- Set1 %>% 
  filter(Genotype %in% c("Col-0", "arf18-3"))

C_lm <- stats::lm(C_per_ug ~ Genotype , data = arf183)
summary(aov(C_lm))

N_lm <- stats::lm(N_per_ug ~ Genotype , data = arf183)
summary(aov(N_lm))

CN_lm <- stats::lm(C_to_N ~ Genotype , data = arf183)
summary(aov(CN_lm))

#### rav2-1
rav2 <- Set1 %>% 
  filter(Genotype %in% c("Col-0", "rav2-1"))

C_lm <- stats::lm(C_per_ug ~ Genotype , data = rav2)
summary(aov(C_lm))

N_lm <- stats::lm(N_per_ug ~ Genotype , data = rav2)
summary(aov(N_lm))

CN_lm <- stats::lm(C_to_N ~ Genotype , data = rav2)
summary(aov(CN_lm))

#### arid5-1
arid5 <- Set1 %>% 
  filter(Genotype %in% c("Col-0", "arid5-1"))

C_lm <- stats::lm(C_per_ug ~ Genotype , data = arid5)
summary(aov(C_lm))

N_lm <- stats::lm(N_per_ug ~ Genotype , data = arid5)
summary(aov(N_lm))

CN_lm <- stats::lm(C_to_N ~ Genotype , data = arid5)
summary(aov(CN_lm))

#### bbx16-1
bbx16 <- Set1 %>% 
  filter(Genotype %in% c("Col-0", "bbx16-1"))

C_lm <- stats::lm(C_per_ug ~ Genotype , data = bbx16)
summary(aov(C_lm))

N_lm <- stats::lm(N_per_ug ~ Genotype , data = bbx16)
summary(aov(N_lm))

CN_lm <- stats::lm(C_to_N ~ Genotype , data = bbx16)
summary(aov(CN_lm))

#### erf107-1
erf107 <- Set1 %>% 
  filter(Genotype %in% c("Col-0", "erf107-1"))

C_lm <- stats::lm(C_per_ug ~ Genotype , data = erf107)
summary(aov(C_lm))

N_lm <- stats::lm(N_per_ug ~ Genotype , data = erf107)
summary(aov(N_lm))

CN_lm <- stats::lm(C_to_N ~ Genotype , data = erf107)
summary(aov(CN_lm))

#### hmgb15-1
hmgb15 <- Set1 %>% 
  filter(Genotype %in% c("Col-0", "hmgb15-1"))

C_lm <- stats::lm(C_per_ug ~ Genotype , data = hmgb15)
summary(aov(C_lm))

N_lm <- stats::lm(N_per_ug ~ Genotype , data = hmgb15)
summary(aov(N_lm))

CN_lm <- stats::lm(C_to_N ~ Genotype , data = hmgb15)
summary(aov(CN_lm))

#### lbd4-1
lbd4 <- Set1 %>% 
  filter(Genotype %in% c("Col-0", "lbd4-1"))

C_lm <- stats::lm(C_per_ug ~ Genotype , data = lbd4)
summary(aov(C_lm))

N_lm <- stats::lm(N_per_ug ~ Genotype , data = lbd4)
summary(aov(N_lm))

CN_lm <- stats::lm(C_to_N ~ Genotype , data = lbd4)
summary(aov(CN_lm))

#### myb29-1
myb29 <- Set1 %>% 
  filter(Genotype %in% c("Col-0", "myb29-1"))

C_lm <- stats::lm(C_per_ug ~ Genotype , data = myb29)
summary(aov(C_lm))

N_lm <- stats::lm(N_per_ug ~ Genotype , data = myb29)
summary(aov(N_lm))

CN_lm <- stats::lm(C_to_N ~ Genotype , data = myb29)
summary(aov(CN_lm))

#### nlp7-1
nlp7 <- Set1 %>% 
  filter(Genotype %in% c("Col-0", "nlp7-1"))

C_lm <- stats::lm(C_per_ug ~ Genotype , data = nlp7)
summary(aov(C_lm))

N_lm <- stats::lm(N_per_ug ~ Genotype , data = nlp7)
summary(aov(N_lm))

CN_lm <- stats::lm(C_to_N ~ Genotype , data = nlp7)
summary(aov(CN_lm))



  
