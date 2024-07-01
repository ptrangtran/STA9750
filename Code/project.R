getwd()
setwd("~/Baruch Fall 2023/STAOPR9750 - Basic Software Tools for Data Analysis/")
smmh <- read.csv("~/Baruch Fall 2023/STAOPR9750 - Basic Software Tools for Data Analysis/smmh.csv", header=TRUE)
View(smmh)
names(smmh)
smmh[,17]<-NULL
colnames(smmh)<- c("Timestamp" ,"Age","Gender","Status","Occuption","Organization","Use_of_Sm","Platforms","Avg_Time","Use_Sm_nopurpose","Distracttion_Sm","Anxiety_No_usingSM","Distract_Scale","Worries_Scale","Concentration","Comparison","Validation","Depressed","Interest_changes","Sleep_quality")
install.packages("regclass")
library(regclass)
class(smmh$`Avg Time`)
factor(smmh$`Avg Time`)
levels(smmh$`Avg Time`)
names(smmh)
smmh$`Avg Time`<- as.factor(smmh$`Avg Time`)
#Model 1
library(RSSL)
m1<- lm(Sleep_quality~Age,data=smmh)
plot(Sleep_quality~Age,data=smmh)
abline(m1,col="green")
associate(Sleep_quality~Age,data=smmh, permutations = 1000)
summary(m1)
m1$fitted.values
plot(m1$residuals~m1$fitted.values,data=smmh)
plot(m1$residuals~smmh$Age,data=smmh)
qqnorm(m1$residuals); qqline(m1$residuals)
plot(m1$residuals, type="p") #time series
abline(h=0, col="blue")
anova(m1)


#Model 2
smmh$Use_of_Sm<- as.factor(smmh$Use_of_Sm)
levels(smmh$Use_of_Sm)
names(smmh)
class(smmh$Use_of_Sm)
summary(smmh$Use_of_Sm)
smmh$Use_of_Sm<- as.numeric(smmh$Use_of_Sm)
m2<- lm(Anxiety_No_usingSM~Use_of_Sm,data=smmh)
plot(Anxiety_No_usingSM~Use_of_Sm,data=smmh)
abline(m2,col="green")
associate(Anxiety_No_usingSM~Use_of_Sm,data=smmh, permutations = 1000)
summary(m2)
m2$fitted.values
plot(m2$residuals~m2$fitted.values,data=smmh)
plot(m2$residuals~smmh$Use_of_Sm,data=smmh)
qqnorm(m2$residuals); qqline(m1$residuals)
plot(m2$residuals, type="p") #time series
abline(h=0, col="blue")
anova(m2)
#Model 3
names(smmh)
smmh$Use_of_Sm<- as.numeric(smmh$Use_of_Sm)
m3<- lm(Concentration~Comparison+Sleep_quality+Age,data=smmh); 
#m3<- lm(Concentration~Comparison+Sleep_quality+Age,data=smmh); confint(m3 ,level=0.95)
plot(Concentration~Comparison,data=smmh)
#possible_regressions(m3)
abline(m3,col="green")
associate(Concentration~Comparison,data=smmh, permutations = 1000)
#summary(m2)
m3$fitted.values
plot(m3$residuals~m3$fitted.values,data=smmh)
plot(m3$residuals~smmh$Age,data=smmh)
qqnorm(m3$residuals); qqline(m3$residuals)
plot(m3$residuals, type="p") #time series
abline(h=0, col="blue")
anova(m3)
summary(m3)
pairs(smmh)
#Model 4
names(smmh)
m4<- lm(Distract_Scale~Use_Sm_nopurpose,data=smmh)
plot(Distract_Scale~Use_Sm_nopurpose,data=smmh)
abline(m4,col="green")
associate(Distract_Scale~Use_Sm_nopurpose,data=smmh, permutations = 1000)
summary(m4)
m4$fitted.values
plot(m4$residuals~m4$fitted.values,data=smmh)
abline(m4,col="green")
plot(m2$residuals~smmh$Use_Sm_nopurpose,data=smmh)
qqnorm(m4$residuals); qqline(m4$residuals)
plot(m4$residuals, type="p") #time series
abline(h=0, col="blue")
anova(m2)

