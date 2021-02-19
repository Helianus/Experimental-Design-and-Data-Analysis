data = read.table(file="birthweight.txt", header=TRUE)
qqnorm(data$birthweight)
data_mean = mean(data$birthweight)
error = qt(0.95, df=length(data$birthweight)) * sd(data$birthweight) / sqrt(length(data$birthweight))
upper_bound = data_mean + error
lower_bound = data_mean - error

t.test(data$birthweight, conf.level=0.9)

t.test(data$birthweight, mu=2800, alternative="greater")