Set1 = read.table("TDNA01_shoot.txt", header=T)

Shoots1 = data.frame(Set1, stringsAsFactors=TRUE )
#make sure the data looks ok
head(Shoots1)
str(Shoots1)

#rosette size day 15
anova_day15 = lm(logDay15 ~ Genotype + Tray, data = Shoots1)
m4=anova(anova_day15)
m4

#rosette size day 22
anova_day22 = lm(logDay22 ~ Genotype + Tray, data = Shoots1)
m3=anova(anova_day22)
m3

#bolting day
anova_bolting = lm(Bolting_transformed ~ Genotype + Tray, data = Shoots1)
m2=anova(anova_bolting)
m2

#flowering day
anova_flowering = lm(Flowering_transformed ~ Genotype + Tray, data = Shoots1)
m1=anova(anova_flowering)
m1
