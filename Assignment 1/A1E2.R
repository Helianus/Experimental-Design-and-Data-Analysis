n = m = 30
mu = 180
nu = 175
sd = 5
B = 1000
p = numeric(B)

for(b in 1:B){
  x = rnorm(n, mu, sd)
  y = rnorm(m, nu, sd)
  p[b] = t.test(x, y, var.equal=TRUE)[[3]]
}
power = mean(p<0.05)

#a
nu = seq(175, 185, by=0.25)
power_a = numeric(length(nu))

for(i in 1:length(nu)){
  for(b in 1:B){
    x = rnorm(n, mu, sd)
    y = rnorm(m, nu[i], sd)
    p[b] = t.test(x, y, var.equal=TRUE)[[3]]
  }
  power_a[i] = mean(p<0.05)
}
plot(nu, power_a, type="l", col="red")

#b
n = m = 100
nu = seq(175, 185, by=0.25)
power_b = numeric(length(nu))

for(i in 1:length(nu)){
  for(b in 1:B){
    x = rnorm(n, mu, sd)
    y = rnorm(m, nu[i], sd)
    p[b] = t.test(x, y, var.equal=TRUE)[[3]]
  }
  power_b[i] = mean(p<0.05)
}
plot(nu, power_b, type="l", col="blue")

#c
n = m = 30
sd = 15
nu = seq(175, 185, by=0.25)
power_c = numeric(length(nu))

for(i in 1:length(nu)){
  for(b in 1:B){
    x = rnorm(n, mu, sd)
    y = rnorm(m, nu[i], sd)
    p[b] = t.test(x, y, var.equal=TRUE)[[3]]
  }
  power_c[i] = mean(p<0.05)
}
plot(nu, power_c, type="l", col="green")

#d
plot(nu, power_a, type="l", col="red")
points(nu, power_b, type="l", col="blue")
points(nu, power_c, type="l", col="green")