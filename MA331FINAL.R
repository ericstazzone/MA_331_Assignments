library(knitr)
library(mosaic)
cscheese=read.csv("C:/Users/essta/Downloads/ex11-53cheese.xls")
favstats(~taste,data=cscheese)
favstats(~acetic,data=cscheese)
favstats(~h2s,data=cscheese)
favstats(~lactic,data=cscheese)
taste=c()
acetic=c()
h2s=c()
lactic=c()
for(x in cscheese[2]){
  taste=c(taste,x)
}
for(x in cscheese[3]){
  acetic=c(acetic,x)
}
for(x in cscheese[4]){
  h2s=c(h2s,x)
}
for(x in cscheese[3]){
  lactic=c(lactic,x)
}
stem(taste)
qqnorm(taste)
densityplot(~taste, data=cscheese)
stem(acetic)
qqnorm(acetic)
densityplot(~acetic, data=cscheese)
stem(h2s)
qqnorm(h2s)
densityplot(~h2s, data=cscheese)
stem(lactic)
qqnorm(lactic)
densityplot(~lactic, data=cscheese)


plot(acetic,taste)
plot(taste,h2s)
plot(lactic,taste)
plot(h2s,acetic)
plot(lactic,acetic)
plot(lactic,h2s)
smallcs=subset(cscheese,select=c("taste","acetic","h2s","lactic"))
with(cscheese,cor(smallcs))
with(cscheese,cor.test(acetic,taste))
with(cscheese,cor.test(taste,h2s))
with(cscheese,cor.test(lactic,taste))
with(cscheese,cor.test(h2s,acetic))
with(cscheese,cor.test(lactic,acetic))
with(cscheese,cor.test(lactic,h2s))

fm1=lm(taste~acetic,data=cscheese)
summary(fm1)
plot(taste~acetic,data=cscheese,main="Taste vs. Acetic")
abline(fm1,col="red")
kable(coefficients(summary(fm1)))
kable(anova(fm1))
plot(residuals.lm(fm1),main="Residual vs. Acetic")
abline(h=c(0),col="red")
plot(density(residuals(fm1)),main="Model 1 (Residual vs. Acetic)")
fm2=lm(taste~h2s,data=cscheese)
fm3=lm(taste~lactic,data=cscheese)
plot(residuals.lm(fm2),main="Residual vs. h2s")
abline(h=c(0),colo="red")
plot(density(residuals(fm2)),main="Model 2 (Residual vs. h2s)")

plot(residuals.lm(fm3),main="Residual vs. Lactic")
abline(h=c(0),colo="red")
plot(density(residuals(fm3)),main="Model 3 (Residual vs. Lactic)")





lm1=lm(taste~acetic+h2s,data=cscheese)
summary(lm1)
kable(coefficients(summary(lm1)))
anova(lm1)
plot(lm1,which=2)
histogram(-residuals(lm1),fit="normal")
plot(lm1,which=1)

lm2=lm(taste~h2s+lactic,data=cscheese)
summary(lm2)
kable(coefficients(summary(lm2)))
anova(lm2)
kable(anova(lm1))
plot(lm2,which=2)
histogram(-residuals(lm1),fit="normal")
plot(lm2,which=1)

lm3=lm(taste~acetic+h2s+lactic,data=cscheese)
summary(lm3)
kable(coefficients(summary(lm3)))
anova(lm3)
kable(anova(lm3))
plot(lm3,which=2)
histogram(-residuals(lm3),fit="normal")
plot(lm3,which=1)

