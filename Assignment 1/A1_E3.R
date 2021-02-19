#a) 
telephone = read.table('telephone.txt', header=T)
med = median(telephone$Bills)
par(mfrow = c(1,3))
hist(telephone$Bills, main='Telephone Bills') # inconsistency: weird shape
boxplot(telephone$Bills)
qqnorm(telephone$Bills)


#b)
mfrow = c(1,1)
lamdaseq = seq(0.01, 0.1, by = 0.002) #why 0.002 here? haven't figured this one out yet :( 
A = numeric(length(lamdaseq))

for (i in 1:length(lamdaseq))
  {B = numeric(1000)
   lambda = lamdaseq[i]
   for (j in 1:length(B))
     {sample = rexp(1000, lambda)
      B[j] = median(sample)
      }
  A[i] = (length(B[B<med])/length(B))
  }

plot(lamdaseq, c(diff(A), 0), type='l')


# c) Construct a 95% bootstrap confidence interval for the population median of the sample.
#computing surrogate dataset, compute T for surrogate dataset
B=1000 
Tstar=numeric(B)
for(i in 1:B) {
  Xstar=sample(telephone$Bills,replace=TRUE) 
  Tstar[i]=median(Xstar) 
}

#95% confidence interval
Tstar25=quantile(Tstar,0.025)
Tstar975=quantile(Tstar,0.975)
ciTstar = c(2*median(Tstar)-Tstar975, median(Tstar), 2*median(Tstar)-Tstar25)

# construct 95% confidence interval from exp. distr. lambda=0.26
B = numeric(1000)
for (j in 1:length(B)){
  sample = rexp(200, 0.026)
  B[j] = median(sample)
}

B25 = quantile(B, 0.025)
B975 = quantile(B, 0.975)
med = median(B)

ciB = c(2*med - B975, 2*med - B25)

# d) test median bill >40 from surrogate dataset
pl = sum(Tstar<40)/B
pr= sum(Tstar>=40)/B
p = 2*(min(pl,pr))

# test percentage bill <10 at most 25% from surrogate dataset
B =1000 
Tstar=numeric(B) 
for(i in 1:B) {
  Xstar=sample(telephone$Bills,replace=TRUE) 
  Tstar[i]=mean(Xstar < 10) 
}

# test median bill <10 
pl = sum(Tstar<0.25)/B
pr= sum(Tstar>=0.25)/B
p = 2*(min(pl,pr))
mean(Tstar)
