#a
data = read.table('titanic.txt', header=T)
attach(data)
survivars_age = Age[Survived == 1]
not_survivars_age = Age[Survived == 0]
boxplot(Age, survivars_age, not_survivars_age)

surv_fem = table(Survived[Sex == "female"])
surv_men = table(Survived[Sex == "male"])

par(mfrow=c(1,2))
barplot(surv_fem)
barplot(surv_men)

surv_1cls = table(Survived[PClass == "1st"])
surv_2cls = table(Survived[PClass == "2nd"])
surv_3cls = table(Survived[PClass == "3rd"])

par(mfrow=c(1,3))
barplot(surv_1cls)
barplot(surv_2cls)
barplot(surv_3cls)

#b
fact_PClass = factor(PClass)
fact_Sex = factor(Sex)
fact_Age = factor(Age)
titglm = glm(Survived ~ fact_PClass + fact_Sex + Age, data = data, family=binomial)
summary(titglm)

exp(3.759662 - 0 - 0 - 0.039177)
exp(3.759662 - 0 - 2.631357 - 0.039177)
exp(3.759662 - 1.291962 - 0 - 0.039177)
exp(3.759662 - 1.291962 - 2.631357 - 0.039177)
exp(3.759662 - 2.521419 - 0 - 0.039177)
exp(3.759662 - 2.521419 - 2.631357 - 0.039177)

#c
glm1 = glm(Survived ~ fact_Sex * fact_PClass * Age, data = data, family=binomial)
anova(glm1, test="Chisq")

titglm2 = glm(Survived ~ fact_PClass + fact_Sex, data = data, family=binomial)
summary(titglm2)

predict(titglm2, data.frame(fact_PClass = "1st", fact_Sex = "male", Age = "53"), type="response")
predict(titglm2, data.frame(fact_PClass = "2nd", fact_Sex = "male", Age = "53"), type="response")
predict(titglm2, data.frame(fact_PClass = "3rd", fact_Sex = "male", Age = "53"), type="response")
predict(titglm2, data.frame(fact_PClass = "1st", fact_Sex = "female", Age = "53"), type="response")
predict(titglm2, data.frame(fact_PClass = "2nd", fact_Sex = "female", Age = "53"), type="response")
predict(titglm2, data.frame(fact_PClass = "3rd", fact_Sex = "female", Age = "53"), type="response")

#d

#e
tot_class = xtabs(~ Survived + fact_PClass)
tot_class
tot_sex = xtabs(~ Survived + fact_Sex)
tot_sex

tot_ch = chisq.test(tot_class)
tot_ch
residuals(tot_ch)
fisher.test(tot_sex)
