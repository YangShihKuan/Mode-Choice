#require(ggplot2)
#require(GGally)
#require(reshape2)
#require(lme4)
#require(compiler)
#require(parallel)
#require(boot)
#require(lattice)

#hdp <- read.csv("c:/Users/Hsun Jung Chen/Desktop/Yang/Travel_Behavior/hdp.csv")
#hdp <- within(hdp, {
  #  Married <- factor(Married, levels = 0:1, labels = c("no", "yes"))
  #  DID <- factor(DID)
  #  HID <- factor(HID)
  #})

#ggpairs(hdp[, c("IL6", "CRP", "LengthofStay", "Experience")])
#ggplot(hdp, aes(x = CancerStage, y = LengthofStay)) +
#  stat_sum(aes(size = ..n.., group = 1)) +
#  scale_size_area(max_size=10)

#m <- glmer(remission ~ IL6 + CRP + CancerStage + LengthofStay + Experience +
#            (1 | DID), data = hdp, family = binomial, control = glmerControl(optimizer = "bobyqa"),
#          nAGQ = 10)

# print the mod results without correlations among fixed effects
#print(m, corr = FALSE)

library(mlogit)
library(AER)

#data("Fishing", package = "mlogit")
#head(Fishing)

#Fish <- mlogit.data(Fishing, shape="wide", varying=2:9, choice="mode")
#Fish

data("TravelMode", package = "AER")
head(TravelMode)

# long shape, the choice is recoceded in variable "choice", alternative variables is "mode"
TM <- mlogit.data(TravelMode, shape = "long", choice = "choice", alt.var = "mode")
#TM

# seven different forumua
f0 <- mFormula(choice ~ vcost + income + size + travel)
f <- mFormula(choice ~ vcost | income + size | travel)
f2 <- mFormula(choice ~ vcost + travel | income + size)
f3 <- mFormula(choice ~ 0 | income)
f4 <- mFormula(choice ~ vcost + travel + wait | income)
f5 <- mFormula(choice ~ vcost | 0 | travel)
f6 <- mFormula(choice ~ vcost | income + 0 | travel)
f7 <- mFormula(choice ~ 0 | income -1 | travel)

#ml.TM <- mlogit(f4, TM,rpar = c(vcost = 'n', travel = 'n'), correlation = TRUE, halton = NA)
ml.TM <- mlogit(f4, TM)
coeftest(ml.TM)

#coeftest(ml.TM)
summary(ml.TM)



#vcov(ml.TM)->vcov.TM #利用vcov()取出所有係數的共變異矩陣
#vcov.TM
#vcov.TM[c("vcost","air:travel"),c("vcost","air:travel")]-> V
#再取出其中屬於我們要的係數的共變異矩陣

#V

#delta_air<-ml.TM$coefficients["air:travel"]
#beta<-ml.TM$coefficients["vcost"]
#g<-delta_air/beta
#dg<-matrix(c(-delta_air/(beta^2),1/beta),2,1)

#Vnew<-t(dg) %*% V %*% dg
#SE_g<-sqrt(Vnew)
#SE_g

# NOT RUN {
#data("Fishing", package = "mlogit")
#library("zoo")
#Fish <- mlogit.data(Fishing, varying = c(2:9), shape = "wide", choice = "mode")
#m <- mlogit(mode ~ price | income | catch, data = Fish)
# compute a data.frame containing the mean value of the covariates in
# the sample
#z <- with(TM, data.frame(vcost = mean(vcost),
#                         travel = mean(travel)))


z <- with(TM, data.frame(vcost = tapply(vcost, index(ml.TM)$alt, mean),
                         travel = tapply(travel, index(ml.TM)$alt, mean),
                         wait = tapply(wait, index(ml.TM)$alt, mean),
                         income = mean(income)))
#z
# compute the marginal effects (the second one is an elasticity
effects(ml.TM, covariate = "income", type = "rr", data = z)
effects(ml.TM, covariate = "vcost", type = "ar", data = z)
effects(ml.TM, covariate = "travel", type = "rr", data = z)
effects(ml.TM, covariate = "wait", type = "ar", data = z)
# }


