rm(list = ls()) ; gc()

library(MASS)
library(ggplot2)
library(gridExtra)
library(grid)

set.seed(1)

df <- read.csv(file.choose())
#lokking for null values
colSums(is.na(df))
head(df)
str(df)

####### data exploration ####

#Number of defaults and non defaults
round(prop.table(table(df$default.payment.next.month)) * 100, digits = 1)

######converted categorical variables into factors and assigned them name 
df$default.payment.next.month <- as.factor(df$default.payment.next.month)
levels(df$default.payment.next.month) <- c("No" , "Yes")

go=ggplot(df, aes(x=default.payment.next.month, fill = default.payment.next.month)) + 
  geom_bar(color="black") + 
  labs(subtitle="Clients Grouped by Default Payment",
       caption="Source: UCI Credit Card",
       x="Default Payment",
       fill="Default Payment")

df$SEX <- as.factor(df$SEX)
levels(df$SEX) <- c("Male","Female")
df$EDUCATION<-ifelse(df$EDUCATION==1,"Graduate School",ifelse(df$EDUCATION==2,"University",
                                                              (ifelse(df$EDUCATION==3,"High School","Others"))))
names<-c("EDUCATION","MARRIAGE")
df[names]<-lapply(df[names],as.factor)

## polting these discrete independent variables

g1 <- ggplot(data=df, aes(x=SEX,fill=SEX)) + geom_bar() +
  labs(title = "Distribution by Gender", x ="Gender",fill = "Gender") +
  scale_fill_manual(values=c("#56B4E9", "#FF9999")) +
  theme(axis.text.x = element_text(angle = 45,hjust=1))

g2 <- ggplot(data=df, aes(x=EDUCATION,fill=EDUCATION)) + geom_bar() +
  labs(title = "Distribution by Education Level", x ="Education Level",fill = "Education Level") +
  theme(axis.text.x = element_text(angle = 45,hjust=1))

g3 <- ggplot(data=df, aes(x=MARRIAGE,fill=MARRIAGE)) + geom_bar() +
  labs(title = "Distribution by Marital Status", x ="Marital Status",fill = "Marital Status") +
  theme(axis.text.x = element_text(angle = 45,hjust=1))
grid.arrange(g1,g2,g3,ncol=2)


#Plotting some discrete independent variables verse response variable

g4=ggplot(data = df, aes(x=SEX, fill = default.payment.next.month)) + 
  geom_bar(position = "dodge", color="black") + 
  labs(title="Grouped by Default Payment",
       x="Gender",       
       fill="Default Payment")+
  theme(axis.text.x = element_text(angle = 45,hjust=1))+
  scale_fill_manual(values = c("skyblue", "navy"))

g5=ggplot(data = df, aes(x=EDUCATION, fill = default.payment.next.month)) + 
  geom_bar(position = "dodge", color="black") + 
  labs(title="Grouped by Default Payment",
       x="Education Level",       
       fill="Default Payment")+
  theme(axis.text.x = element_text(angle = 45,hjust=1))+
  scale_fill_manual(values = c("skyblue", "navy"))

g6=ggplot(data = df, aes(x=MARRIAGE, fill = default.payment.next.month)) + 
  geom_bar(position = "dodge", color="black") + 
  labs(title="Grouped by Default payment",
       x="Marital status",       
       fill="Default Payment")+
  theme(axis.text.x = element_text(angle = 45,hjust=1))+
  scale_fill_manual(values = c("skyblue", "navy"))

grid.arrange(g4,g5,g6,ncol=2)

grid.arrange(g1,g2,g3,g4,g5,g6,ncol=3)



range(df$AGE)
df$ AGE<- as.numeric(df$AGE)
qplot(df$AGE)
qplot(df$LIMIT_BAL)
#Default vs age
ga=ggplot(df, aes(x=AGE, fill = default.payment.next.month)) + 
  geom_bar(color="black") + 
  labs(       subtitle="Clients Grouped by Default Payment Vs AGE",
              caption="Source: UCI Credit Card",
              x="AGE",
              fill="Default Payment")

#default payment vs limit balance
gb=ggplot(data = df, aes(x=LIMIT_BAL, fill = default.payment.next.month)) + 
  geom_bar(position = "dodge", color="black") + 
  labs(subtitle="Clients Grouped by Default Payment",
       caption="Source: UCI Credit Card",
       x="Amount of the given credit",       
       fill="Default Payment")
grid.arrange(ga,gb,ncol=1)




# lokking at column 'PAY' and transforming it and converting it to factor 
PAY_VAR<-lapply(df[,c("PAY_0","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6")], function(x) table(x))
print(PAY_VAR)

#As we see from the results above the number of customers who have delayed payments by 
#5 or more months is very low so we would group them under a single category.
#We observe undocumented values for repayment status variables: -2 and 0. Moreover, fraction of it is 86.5%. Strictly speaking, it is "NAs"

df$PAY_0<-ifelse(df$PAY_0 <1 ,"Paid Duly",ifelse(df$PAY_0==1,"1 month delay",
                                                 ifelse(df$PAY_0==2,"2 month delay",ifelse(df$PAY_0==3,"3 month delay",
                                                   ifelse(df$PAY_0==4,"4 month delay","5 month or more delay")))))

df$PAY_2<-ifelse(df$PAY_2 <1 ,"Paid Duly",ifelse(df$PAY_2==1,"1 month delay",
                                                 ifelse(df$PAY_2==2,"2 month delay",ifelse(df$PAY_2==3,"3 month delay",
                                                 ifelse(df$PAY_2==4,"4 month delay","5 month or more delay")))))

df$PAY_3<-ifelse(df$PAY_3 <1 ,"Paid Duly",ifelse(df$PAY_3==1,"1 month delay",
                                                 ifelse(df$PAY_3==2,"2 month delay",ifelse(df$PAY_3==3,"3 month delay",
                                                  ifelse(df$PAY_3==4,"4 month delay","5 month or more delay")))))
df$PAY_4<-ifelse(df$PAY_4 <1 ,"Paid Duly",ifelse(df$PAY_4==1,"1 month delay",
                                                 ifelse(df$PAY_4==2,"2 month delay",ifelse(df$PAY_4==3,"3 month delay",
                                                   ifelse(df$PAY_4==4,"4 month delay","5 month or more delay")))))

df$PAY_5<-ifelse(df$PAY_5 <1 ,"Paid Duly",ifelse(df$PAY_5==1,"1 month delay",
                                                 ifelse(df$PAY_5==2,"2 month delay",ifelse(df$PAY_5==3,"3 month delay",
                                                  ifelse(df$PAY_5==4,"4 month delay","5 month or more delay")))))

df$PAY_6<-ifelse(df$PAY_6 <1 ,"Paid Duly",ifelse(df$PAY_6==1,"1 month delay",
                                                 ifelse(df$PAY_6==2,"2 month delay",ifelse(df$PAY_6==3,"3 month delay",
                                                 ifelse(df$PAY_6==4,"4 month delay","5 month or more delay")))))
names<-c("PAY_0","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6")
df[names]<-lapply(df[names],as.factor)
str(df)
head(df)


P0=ggplot(data = df, aes(x=PAY_0, fill = PAY_0)) + 
  geom_bar(position = "dodge", color="black") + 
  labs(title="Bar plot", 
       caption="Source: UCI Credit Card",
       x="PAY_0",       
       fill="PAY_0")+
  scale_fill_manual(values = c("skyblue", "navy","green","blue","yellow","grey"))
P2=ggplot(data = df, aes(x=PAY_2, fill = PAY_2)) + 
  geom_bar(position = "dodge", color="black") + 
  labs(title="Bar plot", 
       caption="Source: UCI Credit Card",
       x="PAY_2",       
       fill="PAY_2")+
  scale_fill_manual(values = c("skyblue", "navy","green","blue","yellow","grey"))
P3=ggplot(data = df, aes(x=PAY_3, fill = PAY_3)) + 
  geom_bar(position = "dodge", color="black") + 
  labs(title="Bar plot", 
       caption="Source: UCI Credit Card",
       x="PAY_3",       
       fill="PAY_3")+
  scale_fill_manual(values = c("skyblue", "navy","green","blue","yellow","grey"))
P4=ggplot(data = df, aes(x=PAY_4, fill = PAY_4)) + 
  geom_bar(position = "dodge", color="black") + 
  labs(title="Bar plot", 
       caption="Source: UCI Credit Card",
       x="PAY_4",       
       fill="PAY_4")+
  scale_fill_manual(values = c("skyblue", "navy","green","blue","yellow","grey"))
P5=ggplot(data = df, aes(x=PAY_5, fill = PAY_5)) + 
  geom_bar(position = "dodge", color="black") + 
  labs(title="Bar plot", 
       caption="Source: UCI Credit Card",
       x="PAY_5",       
       fill="PAY_5")+
  scale_fill_manual(values = c("skyblue", "navy","green","blue","yellow","grey"))
P6=ggplot(data = df, aes(x=PAY_6, fill = PAY_6)) + 
  geom_bar(position = "dodge", color="black") + 
  labs(title="Bar plot", 
       caption="Source: UCI Credit Card",
       x="PAY_6",       
       fill="PAY_6")+
  scale_fill_manual(values = c("skyblue", "navy","green","blue","yellow","grey"))
grid.arrange(P0,P2,P3,P4,P5,P6,ncol=3)







### normalizing data
ind <- sapply(df, is.numeric)
df[ind] <- lapply(df[ind], scale)
head(df)

##### checking coleration
cor(df[ind])
library(corrgram)


### We see a high level of linear correlations between the amount of bill statements in different months i-e BILL_AMT1 TO BILL_AMT6


########################################################################

#splitting dataset into 70 % training and 20 % testing
data2 = sort(sample(nrow(df), nrow(df)*.7))

#creating training data set by selecting the output row values
train <- df[data2,]
dim(train)

#creating test data set by not selecting the output row values
test <- df[-data2,]
dim(test)


#Fitting logistic regression with all the variables
set.seed(1)
log.model <- glm(default.payment.next.month ~., data = train, family = binomial(link = "logit"))
summary(log.model)
#Prediction
log.predictions <- predict(log.model, test, type="response")
log.prediction.rd <- ifelse(log.predictions > 0.5, 'Yes', 'No')
#confusion table
table(log.prediction.rd, test[,25])
#accuracy
accuracy <- table(log.prediction.rd, test[,25])
sum(diag(accuracy))/sum(accuracy)
tp=accuracy[2,2]
fn=accuracy[1,2]
tn=accuracy[1,1]
fp=accuracy[2,1]
# Precision TP / (TP + FP)
tpsumfp=tp+fp
precision=tp/tpsumfp
precision

#Recall TP / (TP + FN)
tpsumfn=tp+fn
recall=tp/tpsumfn
recall

#F1 score
F1=2*precision*recall/(precision+recall)
F1


##### Finding best parametersusing 1) AIC 2) looking at p value from wald statistic, alpha=0.05

#1) AIC
step_AIC<-stepAIC(log.model,direction='backward',trace=FALSE)
step_AIC$anova


set.seed(2)
log.model2 <- glm(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + 
                     AGE + PAY_0 +  PAY_5 + PAY_6 + PAY_AMT1 + PAY_AMT2 , data = train, family = binomial(link = "logit"))
summary(log.model2)
#Prediction
log.predictions2 <- predict(log.model2, test, type="response")

log.prediction.rd2 <- ifelse(log.predictions2 > 0.5, 'Yes', 'No')
#confusion table
table(log.prediction.rd2, test[,25])
#accuracy
accuracy2 <- table(log.prediction.rd2, test[,25])
sum(diag(accuracy2))/sum(accuracy2)

tp=accuracy2[2,2]
fn=accuracy2[1,2]
tn=accuracy2[1,1]
fp=accuracy2[2,1]
# Precision TP / (TP + FP)
tpsumfp=tp+fp
precision=tp/tpsumfp
precision
#Recall TP / (TP + FN)
tpsumfn=tp+fn
recall=tp/tpsumfn
recall
#F1 score
F1=2*precision*recall/(precision+recall)
F1

##### varaiable selection based on wald statistics
set.seed(10)
log.model3 <- glm(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE +AGE + PAY_0 +PAY_5 + PAY_6 +
                   BILL_AMT6+PAY_AMT1 + PAY_AMT2 +PAY_AMT6 , data = train, family = binomial(link = "logit"))
summary(log.model3)
#Prediction
log.predictions3 <- predict(log.model3, test, type="response")
log.prediction.rd3 <- ifelse(log.predictions3 > 0.5, 'Yes', 'No')
#confusion table
table(log.prediction.rd3, test[,25])


#accuracy
accuracy3 <- table(log.prediction.rd3, test[,25])
sum(diag(accuracy3))/sum(accuracy3)

tp=accuracy3[2,2]
fn=accuracy3[1,2]
tn=accuracy3[1,1]
fp=accuracy3[2,1]
# Precision TP / (TP + FP)
tpsumfp=tp+fp
precision=tp/tpsumfp
precision
#Recall TP / (TP + FN)
tpsumfn=tp+fn
recall=tp/tpsumfn
recall

#F1 score
F1=2*precision*recall/(precision+recall)
F1

