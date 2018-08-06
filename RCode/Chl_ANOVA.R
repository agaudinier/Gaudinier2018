##Chlorophyll Content Analysis

##this chlorophyll assay was by reading the chlorophyll content index 
#using CCM-200 plus (Opti-Sciences) 

Set1 = read.csv("~/Desktop/WholePlantPhenotyping/Chl_meter/allgenotypes_m_8_3_17.csv", header=T)
#Root = tibble(Set1)
Chl = data.frame(Set1, stringsAsFactors=TRUE )
library(lsmeans)

geno <- c("gnc", "arf18-3" , "bbx16-1", "erf107-1", "rav2-1", "arid5-1","hat22-1", "anac032-1", "Col-0", 
          "nac102-1", "hmgb15-1", "arf18-2", "myb29-1", "arf9-2","lbd4-1")
Chl$Genotype <- factor(Chl$Genotype, levels = geno)

head(Chl)
anova_Chl = lm(CCI ~ Genotype, data = Chl)
m1=anova(anova_Chl)
m1

library(tidyverse)
Chl <- as_tibble(Chl)

subset <- Chl %>%
  filter(Genotype %in% c("Col-0", "lbd4-1"))
  
anova_Chl = lm(CCI ~ Genotype, data = subset)
m1=anova(anova_Chl)
m1

ggplot (data = Chl, mapping = aes( x = Genotype, y = CCI, fill = Genotype)) +
  geom_boxplot(position = position_dodge(width=0.9),outlier.shape=NA) +
  geom_point(position=position_jitterdodge(dodge.width=.99), size = 1, alpha =.7) +
  scale_fill_manual(values=c("#9ACC30", "white", "white","white","white","white",
                             "white","white", "gray", "white", "white", "white",
                             "white", "white", "#548B54")) +
  labs(y = "Chlorophyll Content Index") +
  theme_light() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(legend.position="none")
#____________________________________________________

##this chlorophyll assay was performed with leaf discs

Set1 = read.csv("~/Desktop/WholePlantPhenotyping/Chl_meter/46and47discs_8_3_17.csv", header=T)
#Root = tibble(Set1)
Chl = data.frame(Set1, stringsAsFactors=TRUE )
library(lsmeans)

head(Chl)
anova_Chl = lm(CCI ~ Genotype, data = Chl)
m1=anova(anova_Chl)
m1

library(tidyverse)

Chl <- as_tibble(Chl)

subset <- Chl %>%
  filter(Genotype %in% c("Col-0", "chl1-5"))
subset

anova_Chl = lm(Chls ~ Genotype, data = subset)
m1=anova(anova_Chl)
m1

geno <- c("nlp7-1", "Col-0", "chl1-5")
Chl$Genotype <- factor(Chl$Genotype, levels = geno)

ggplot (data = Chl, mapping = aes( x = Genotype, y = Chls, fill = Genotype)) +
  geom_boxplot(position = position_dodge(width=0.9),outlier.shape=NA) +
  geom_point(position=position_jitterdodge(dodge.width=.99), size = 1, alpha =.7) +
  scale_fill_manual(values=c("#9ACC30", "gray", "white")) +
  labs(y = "Total Chlorophyll (Chls)") +
  theme_light() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(legend.position="none")
