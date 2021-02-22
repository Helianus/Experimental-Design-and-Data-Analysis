#a
data_4 = read.table(file="run.txt", header=TRUE)
hist(data_4$before)
hist(data_4$after)

boxplot(data_4$before, data_4$after)
plot(data_4$before - data_4$after)
boxplot(data_4$before - data_4$after)

t.test(data_4$before, data_4$after, paired=TRUE)
t.test(data_4$before - data_4$after)

qqnorm(data_4$before - data_4$after)
shapiro.test(data_4$before - data_4$after)

#b

energy_data_before = data_4$before[data_4[,3] == "energy"]
energy_data_after = data_4$after[data_4[,3] == "energy"]
boxplot(energy_data_before, energy_data_after)
boxplot(energy_data_before - energy_data_after)

par(mfrow=c(1,2))
hist(energy_data_before)
hist(energy_data_after)

t.test(energy_data_before, energy_data_after, paired=TRUE)
t.test(energy_data_before - energy_data_after)
par(mfrow=c(1,1))
qqnorm(energy_data_before - energy_data_after)


lemo_data_before = data_4$before[data_4[,3] == "lemo"]
lemo_data_after = data_4$after[data_4[,3] == "lemo"]
boxplot(lemo_data_before, lemo_data_after)
boxplot(lemo_data_before - lemo_data_after)

par(mfrow=c(1,2))
hist(lemo_data_before)
hist(lemo_data_after)

t.test(lemo_data_before, lemo_data_after, paired=TRUE)
t.test(lemo_data_before - lemo_data_after)
par(mfrow=c(1,1))
qqnorm(lemo_data_before - lemo_data_after)

#c
child_time = numeric(length(data_4$before))
for(i in 1:length(data_4$before)){
  child_time[i] =  data_4$before[i] - data_4$after[i]
}

lemo_child_time = child_time[data_4[,3] == "lemo"]
energy_child_time = child_time[data_4[,3] == "energy"]

hist()