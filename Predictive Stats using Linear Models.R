#Q1
setwd("~/Documents/BIOMETRY/week10")
coffee <-round(runif(200, 180, 300),1)
tea <-round(runif(200, 2130, 3050), 1)
soda <-runif(1, 1.8, 3.2)
water <-runif(1, 0.28, 0.38)
butterbeer <-runif(1, 15, 23)
soundsnasty <- runif(200,3,9)+butterbeer+soda*coffee+water*tea
realnasty <-round(soundsnasty-min(soundsnasty),0)
dat.Q1 <-data.frame(realnasty, coffee, tea)
colnames(dat.Q1) <-c("num.seeds", "days.without.rain", "elevation")
head(dat.Q1)
num.seeds days.without.rain elevation
1       371             255.4    2994.6
2       253             183.8    3041.7
3        78             230.9    2141.1
4       353             299.3    2645.8
5       154             250.9    2276.7
6       199             194.8    2782.4
cor(dat.Q1)
num.seeds days.without.rain elevation
num.seeds         1.0000000         0.6981271 0.7993179
days.without.rain 0.6981271         1.0000000 0.1281271
elevation         0.7993179         0.1281271 1.0000000

#Q2
silverlm <- lm(num.seeds ~ days.without.rain + elevation , data = dat.Q1)
plot(silverlm)
Hit <Return> to see next plot: 
  Hit <Return> to see next plot: 
  Hit <Return> to see next plot: 
  Hit <Return> to see next plot: 
library(car)
Loading required package: carData
Anova(silverlm, type = 2)
Anova Table (Type II tests)

Response: num.seeds
Sum Sq  Df F value    Pr(>F)    
days.without.rain  775398   1  241673 < 2.2e-16 ***
  elevation         1101050   1  343171 < 2.2e-16 ***
  Residuals             632 197                      
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
library(AER)
Loading required package: lmtest
Loading required package: zoo

Attaching package: ‘zoo’

The following objects are masked from ‘package:base’:
  
  as.Date, as.Date.numeric

Loading required package: sandwich
Loading required package: survival
coeftest(silverlm, vcov=vcovHC(silverlm, type = 'HC1'))

t test of coefficients:
  
  Estimate  Std. Error t value  Pr(>|t|)    
(Intercept)       -9.6607e+02  1.4400e+00 -670.90 < 2.2e-16 ***
  days.without.rain  1.8364e+00  3.6962e-03  496.83 < 2.2e-16 ***
  elevation          2.8966e-01  4.7573e-04  608.87 < 2.2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
summary(silverlm)

Call:
  lm(formula = num.seeds ~ days.without.rain + elevation, data = dat.Q1)

Residuals:
  Min      1Q  Median      3Q     Max 
-3.1811 -1.4939  0.0737  1.3900  3.1250 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)       -9.661e+02  1.450e+00  -666.5   <2e-16 ***
  days.without.rain  1.836e+00  3.736e-03   491.6   <2e-16 ***
  elevation          2.897e-01  4.945e-04   585.8   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.791 on 197 degrees of freedom
Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997 
F-statistic: 3.348e+05 on 2 and 197 DF,  p-value: < 2.2e-16
library(heplots)
etasq(silverlm)
Partial eta^2
days.without.rain     0.9991855
elevation             0.9994263
Residuals                    NA
library(broom)
data(mtcars)
test <- lm(mpg ~ wt, mtcars)
results <- tidy(test)

results

term  estimate std.error statistic      p.value
1 (Intercept) 37.285126  1.877627 19.857575 8.241799e-19
2          wt -5.344472  0.559101 -9.559044 1.293959e-10
From there you can extract your desired values into new variables:
  
  a <- results$estimate[1]
b <- results$estimate[2]

summary(fit)$coefficients[1,1]
  
  summary(fit)$coefficients[2,1]

#Q4
  s.dat <-dat.Q1[order(dat.Q1$num.seeds),]
  new.sites <-rbind.data.frame(s.dat[sample(1:10, 1),2:3],
                                 +                              s.dat[sample(150:185, 1),2:3])
  rownames(new.sites) <-c("site.1", "site.2")
  head(new.sites)
  days.without.rain elevation
  site.1             206.4    2171.1
  site.2             259.9    2751.2
#Q5
  names(summary(silverlm))
  [1] "call"          "terms"         "residuals"     "coefficients"  "aliased"       "sigma"         "df"           
  [8] "r.squared"     "adj.r.squared" "fstatistic"    "cov.unscaled" 
  > summary(silverlm)$coefficients[1,1]
  [1] -966.0727
  summary(silverlm)$coefficients[2,1]
  [1] 1.836394
  coef(silverlm)
  (Intercept) days.without.rain         elevation 
  -966.0727063         1.8363941         0.2896612 
 predict.lm(silverlm, new.sites, level = 0.95, type = c('response') )
  site.1    site.2 
  41.84254 308.12210 
  site1 <- (1.836*206.4) + (0.2896612*2171.1) + -966.0727063
  site1
  [1] 41.76113
  site2 <- (1.836*259.9) + (0.2896612*2751.2) + -966.0727063
  site2
  [1] 308.0196
  #Q6
  prediction <- predict.lm(silverlm, new.sites, level = 0.95, type = c('response') )
  prediction
  site.1    site.2 
  41.84254 308.12210 
  cbind(data.frame(prediction))
          prediction
  site.1   41.84254
  site.2  308.12210
  obj = cbind(data.frame(prediction))
  colnames(obj) <- c('num.seeds.predcited')
  obj
            num.seeds.predcited
  site.1            41.84254
  site.2           308.12210
  View(dat.Q1)
  colnames(obj) <- c('num.seeds')
  obj
          num.seeds
  site.1  41.84254
  site.2 308.12210
  cbind(new.sites, obj)
          days.without.rain elevation num.seeds
  site.1             206.4    2171.1  41.84254
  site.2             259.9    2751.2 308.12210
  site.pred <- cbind(new.sites, obj)
  data.frame(site.pred)
  days.without.rain elevation num.seeds
  site.1             206.4    2171.1  41.84254
  site.2             259.9    2751.2 308.12210
  rbind.data.frame(dat.Q1, site.pred)
  num.seeds days.without.rain elevation
  1      233.00000             192.1    2724.5
  199    613.00000             288.3    2954.8
  200    252.00000             266.1    2131.4
  site.1  41.84254             206.4    2171.1
  site.2 308.12210             259.9    2751.2
  dat.Q1prediction <- rbind.data.frame(dat.Q1, site.pred)
  tail(dat.Q1prediction)
  num.seeds days.without.rain elevation
  197    236.00000             190.0    2749.0
  198     84.00000             210.6    2144.0
  199    613.00000             288.3    2954.8
  200    252.00000             266.1    2131.4
  site.1  41.84254             206.4    2171.1
  site.2 308.12210             259.9    2751.2
  #Q7P1
  ggplot(dat.Q1prediction, aes(x=days.without.rain, y=num.seeds, color='red')) + geom_point(size=2, shape=23) + geom_smooth(method=lm) + labs(title = "                                                                           Number of Seeds vs Days without Rain") + stat_ellipse()
  tingwei <- ggplot(dat.Q1prediction, aes(x=days.without.rain, y=num.seeds, color='red')) + geom_point(size=2, shape=23) + geom_smooth(method=lm) + labs(title = "                                                                           Number of Seeds vs Days without Rain") + stat_ellipse() + geom_text()
  tingwei <- ggplot(dat.Q1prediction, aes(x=days.without.rain, y=num.seeds, color='red', label = num.seeds)) + geom_point(size=2, shape=23) + geom_smooth(method=lm) + labs(title = "                                                                           Number of Seeds vs Days without Rain") + stat_ellipse() + geom_text()
  tingwei <- ggplot(dat.Q1prediction, aes(x=days.without.rain, y=num.seeds, color='red', label = num.seeds)) + geom_point(size=2, shape=23) + geom_smooth(method=lm) + labs(title = "                                                                           Number of Seeds vs Days without Rain") + stat_ellipse() 
  tingwei + geom_text()
  tingwei + geom_text(check_overlap = T)
  tingwei + geom_label()
  tingwei + geom_label(x=206.4, y=41.825372760865, label='site.1', color='green', check_overlap = T) + geom_label(x=259.9, y=308.122102384419, label='site.2', color='blue', check_overlap = T)
  Warning: Ignoring unknown parameters: check_overlap
  Warning: Ignoring unknown parameters: check_overlap
  tingwei + geom_label(x=206.4, y=41.825372760865, label='site.1', color='black', check_overlap = T) + geom_label(x=259.9, y=308.122102384419, label='site.2', color='blue', check_overlap = T)
  Warning: Ignoring unknown parameters: check_overlap
  Warning: Ignoring unknown parameters: check_overlap
  tingwei + geom_label(x=206.4, y=41.825372760865, label='site.1', color='green', size = 6, check_overlap = T) + geom_label(x=259.9, y=308.122102384419, label='site.2', color='blue', size = 6, check_overlap = T)
  Warning: Ignoring unknown parameters: check_overlap
  Warning: Ignoring unknown parameters: check_overlap
  tingwei + geom_label(x=206.4, y=41.825372760865, label='site.1', color='red', size = 6, check_overlap = T) + geom_label(x=259.9, y=308.122102384419, label='site.2', color='green', size = 6, check_overlap = T)
  Warning: Ignoring unknown parameters: check_overlap
  Warning: Ignoring unknown parameters: check_overlap
  #Q7P2
  ggplot(dat.Q1prediction, aes(x=elevation, y=num.seeds)) + geom_point(size=2, shape=25) + geom_smooth(method=lm) + labs(title = "                                                                           Number of Seeds vs elevation") + stat_ellipse()
  tingwei2 <- ggplot(dat.Q1prediction, aes(x=elevation, y=num.seeds)) + geom_point(size=2, shape=25) + geom_smooth(method=lm) + labs(title = "                                                                           Number of Seeds vs elevation") + stat_ellipse()
  tingwei2 + geom_label(x=2171.1, y=41.825372760865, label='site.1', color='red', size = 6, check_overlap = T) + geom_label(x=2751.2, y=308.122102384419, label='site.2', color='green', size = 6, check_overlap = T)
  Warning: Ignoring unknown parameters: check_overlap
  Warning: Ignoring unknown parameters: check_overlap
  #Q8
  > In Notes 
  
  