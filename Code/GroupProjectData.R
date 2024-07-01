socialmediadata <- read.csv("~/Baruch Fall 2023/STAOPR9750 - Basic Software Tools for Data Analysis/mmc2.csv") 
getwd()
library(readxl)
#socialmediadata <- read_excel("~/Desktop/STAOPR9750/mmc2.xlsx")
data.frame(socialmediadata)
names(socialmediadata)
socialmediadata[,2] <- NULL
socialmediadata$`4. Which type of internet connection do you use?`<- NULL
socialmediadata$`23. Marital Status`<- NULL
socialmediadata$`24. Religion` <- NULL
socialmediadata$`30. Body weight (Kg)`<- NULL
socialmediadata$`31. Height (m)`<- NULL
socialmediadata[,56:69] <- NULL
socialmediadata$`3. Which device do you usually use to connect social media?`<- NULL
socialmediadata$`18. Have you ever compared yourself with other’s success or luxurious life?`<- NULL
socialmediadata$`20. If answer is yes, are you trying to control that thing and trying to reduce the use of social media?`<- NULL
socialmediadata[,26:32] <- NULL
socialmediadata[,31:33] <- NULL
socialmediadata[,34:38] <- NULL
socialmediadata[,35:36] <- NULL
View(socialmediadata)
names(socialmediadata)
#Renaming columns
colnames(socialmediadata)[2] <- "Social Media?"
colnames(socialmediadata)[3] <- "How_longSM"
colnames(socialmediadata)[4] <- "Frequency of Posting?"
colnames(socialmediadata)[5] <- "Time_Spent"
colnames(socialmediadata)[6:17] <-c("Use Frequency","N friends","Known friend","Groups in SM","Purpose of SM","Content type","SM is good?","Believe in SM news?","Pressure by SM?","Feel influenced?","Mental_Health","Age?")
colnames(socialmediadata)[18:24]<-c("Gender","Education","Profession","Monthly income","Area of residence","Living with","Smoking habit")
colnames(socialmediadata)[25:36]<- c("Feeling_lonely","Lack of interest?","Depressed_Hopeless","Insomnia","Tired","Appetite?","Suicidal toughts","Anxiety?","Always worried?","Bedtime?","Sleeping_time","Sleep_quality")
names(socialmediadata)
View(socialmediadata)
lapply(socialmediadata,class)
socialmediadata[1:36]<- lapply(socialmediadata[1:36],factor)
lapply(socialmediadata,class)
summary(socialmediadata$Profession)
#Association
associate(Mental_Health~Feeling_lonely,data=socialmediadata,permutations = 1000)
associate(Mental_Health~Depressed_Hopeless,data=socialmediadata,permutations = 1000)
associate(Mental_Health~Insomnia,data=socialmediadata,permutations = 1500)
associate(Mental_Health~How_longSM,data=socialmediadata,permutations = 1500)
associate(Mental_Health~Sleeping_time,data=socialmediadata,permutations = 1500)

#plot
library(regclass)
plot(Mental_Health~Feeling_lonely,data=socialmediadata)
lm(Mental_Health~Feeling_lonely,data=socialmediadata)

levels(socialmediadata$Mental_Health)
class(socialmediadata$Mental_Health)
as.numeric(socialmediadata$Mental_Health)
as.numeric(socialmediadata$How_longSM)
lmproject<-lm(NumbMentalHealth~NumbHowLongSM,data=socialmediadata)
plot(NumbMentalHealth~socialmediadata$`Age?`,data=socialmediadata)  
#lm(NumbMentalHealth~socialmediadata$`Age?`,data=socialmediadata) #NO         
socialmediadata$NumbMentalHealth<-as.numeric(socialmediadata$Mental_Health)
socialmediadata$NumbHowLongSM<-as.numeric(socialmediadata$How_longSM)
summary(lmproject)
names(socialmediadata)
socialmediadata$NumbFeelingLonely<-as.numeric(socialmediadata$Feeling_lonely)
plot(NumbHowLongSM~NumbFeelingLonely,data=socialmediadata)

summary(socialmediadata$NumbMentalHealth)
table(socialmediadata$NumbHowLongSM)

library(readr)
smedia2 <- read_csv("~/Baruch Fall 2023/STAOPR9750 - Basic Software Tools for Data Analysis/smedia2.csv")

View(smedia2)
smedia2[1:177,2]
newmodel<- socialmediadata[1:177,] 
newmodel$sm2<-smedia2[1:177,2]
names(newmodel)
View(newmodel)
class(newmodel$sm2$Facebook)
lm(newmodel$How_longSM~newmodel$sm2, data= newmodel)
plot(sm2$Facebook~newmodel$Timestamp, data= newmodel)
lm(sm2$Facebook~newmodel$Timestamp, data= newmodel)
lm1<- lm(sm2$Facebook~newmodel$Timestamp, data= newmodel)
summary(lm1)
summary(sm2$Facebook~newmodel$Timestamp, data= newmodel)
nrow(socialmediadata)
nrow(smedia2)
