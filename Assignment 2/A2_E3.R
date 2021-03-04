#a
cow = read.table('cow.txt', header=T); cow$id= factor(cow$id); cow$per = factor(cow$per)
cowlm = lm(milk ~ id + per + treatment, data = cow)
anova(cowlm)
summary(cowlm)
# p-value is big 0.516 so this means that the treatment is not that important

#b
library(lme4)
cowlmer = lmer(milk ~ per + treatment + (1|id), data = cow, REML = FALSE)
cowlmer1 = lmer(milk ~ per + (1|id), data = cow, REML = FALSE)
anova(cowlmer1, cowlmer)
# p-value is big 0.446 so this means that the treatment is not that important

#c
attach(cow)
t.test(milk[treatment=="A"], milk[treatment=="B"],paired=TRUE)
# p-value is big 0.828 so this means that the treatment is not that important