## Exercise 1
install.packaes("car")
library(car)

bread = read.table(file="bread.txt", header=TRUE, sep=" ")
attach(bread)

# a) randomization of the 18 slices into the 6 combinations of conditions (3 environments, 2 types of humidity)
env=3; hum=2; N=3
rbind(rep(1:environment,each=N*humidity),rep(1:humidity,N*environment),sample(1:(N*environment*humidity)))

#Interpreting the table resulting from the randomization process:
# each column can be seen as a different unit (where the ID of the unit is the value in the third row)
# the first row can be seen as the environment that each unit has to be measured in
# the second row can be seen as the humidity group of the unit. 

# b) boxplots of hours versus the 2 factors & 2 interaction plots (keeping the 2 factors fixed in turn)
par(mfrow=c(1,2))
boxplot(hours~environment, main = "Effect of environment")
boxplot(hours~humidity, main = "Effect of humidity")

par(mfrow=c(1,2))
interaction.plot(environment,humidity,hours, main="Environment vs. Humidity")
interaction.plot(humidity,environment,hours, main="Humidity vs. Environment")

# c) Variance analysis for the effect of the factors (environment & humidity) and their interaction
aovhoursenv=lm(hours~environment*humidity)
anova(aovhoursenv)
summary(aovhoursenv)
summary(anova(aovhoursenv))

aovhourshum=lm(hours~humidity+environment)
anova(aovhourshum)
summary(aovhourshum)
# When performing a one way ANOVA test to analyse the effect of the factors on the decay and their interaction, it can be concluded that:
# Both factors have a significant effect on the time of the decay: F=131.9, p-value: 4.676e-10 

qqnorm(residuals(aovhoursenv))
plot(fitted(aovhoursenv),residuals(aovhoursenv), main='Interaction plot')
#When analysing the graphical representation the interaction (the interaction plot), the interaction of Humidity vs. Environment shows a clear effect:
# it can be concluded that for both intermediate and warm environments, the mean of the hours of decay is decreasing when humidity is increasing (from a dry to a wet environment).
# For the cold environment, humidity has an opposite effect - an increase in the duration of the time to decay once humidity increases (from a dry to a wet environment).
# Furthermore, the spread in the residuals seems to be bigger for middle-fitted values, while a few data points seem extreme, requiring further outliers investigation.



# d) Which factor has a greater (numerical) influence on the decay?

#Interaction plot with Friedman test
duration=as.vector(as.matrix(bread[1:18,]))
id=as.factor(rep(1:18,4))
environment=as.factor(rep(1:3,each=6))
humidity=as.factor(rep(1:2,each=3))
breaddata=data.frame(cbind(duration,id,environment,humidity))
breaddata[1:18,] #??????

boxplot(duration~humidity, xlab="humidity", ylab="duration")
interaction.plot()

friedman.test(duration,environment,id,data=breaddata)


# e) Check model assumptions & outliers
#To verify the normality and the model assumptions, a QQ-plot of the residuals is analysed, together with the fitted values.

par(mfrow=c(1,2))
qqnorm(residuals(aovhoursenv),main = "QQ-plot residuals"); 
plot(fitted(aovhoursenv),residuals(aovhoursenv), main = "Residuals against fitted values")

shapiro.test(residuals(aovhoursenv))
# Shapiro-Wilk normality test results lead to a p-value = 0.1911, which means the normality assumption is not rejected: there is no reason to suspect that the differences are not resulting from a normal distribution.

