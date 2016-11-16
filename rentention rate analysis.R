library(xlsx)
library(mice)
library(dplyr)
library(Hmisc)
library(ggplot2)
library(caret)
library(class)
library(rattle)
library(rpart.plot)

setwd("/Users/weizeng/Documents/UberTakehome/")
df<-read.csv("T&S_Advanced Analytics Dataset.csv",stringsAsFactors = FALSE)

#DATA OVERVIEW
dim(df)
summary(df)
names(df)

#manipulate datess
class(df$signup_date)
df$last_trip_date=  strptime(df$last_trip_date, format='%m/%d/%y')
df$signup_date=     strptime(df$signup_date, format='%m/%d/%y')

#check the data collection periods 
print (paste("Min Date of Last Trip:", min(df$last_trip_date), "Max Date of Last Trip:", max(df$last_trip_date)))
print (paste("Min SignUp Date:",min(df$signup_date), "Max SignUp Date:",max(df$signup_date)))


#check missing pattern
md.pattern(df)

#define whether a case is active or not according whether he/she took a trip 30 days before the data_pulldate which assumes to be the max (last_trip_date)

data_pulldate=max(df$last_trip_date)
df$active<-with(df, ifelse(difftime(data_pulldate, last_trip_date,units="days")<30,1,0))


#check correlation for numberic variables
nums <- sapply(df, is.numeric)
df_num<-df[,nums]
sapply(df_num,cor(),df_num$active)
df3_num<-na.omit(df_num)
cor( df3_num)


#Exploring the covariates by dividing them into some sub-groups; Additional codes for exploring were cleaned
df$numoftrips_grp<-cut(df$trips_in_first_30_days,c(-1,0,1,2,3,4,5,10,max(df$trips_in_first_30_days)))
df$ratingofdriver_grp<-addNA(cut(df$avg_rating_of_driver,c(1,2,3,4,4.5,4.9,5)))
df$ratingbydriver_grp<-addNA(cut(df$avg_rating_by_driver,c(1,2,3,4,4.5,4.9,5)))
df$avgdist_grp<-cut2(df$avg_dist,g=8)
df$surgepct_grp<-(cut2(df$surge_pct,g=2))
df$avg_surge_grp<-(cut(df$avg_surge,c(0,1,1.05,1.1,1.2,max(df$avg_surge))))
df$wkdaypct_grp<-(cut2(df$weekday_pct,g=5))


#calculate the retention rates by sub-groups
varlist<-c("city","numoftrips_grp","phone","ratingofdriver_grp","ratingbydriver_grp","avgdist_grp","surgepct_grp","avg_surge_grp","uber_black_user","wkdaypct_grp")
activerate<-data.frame()

  for (m in varlist) {
    position<-grep(m, colnames(df),1)
    print(position)
    tmp<-as.data.frame(prop.table(table(df$active, df[,position]),2))
    tmp$Var<-m
    activerate<-rbind(activerate, filter(tmp, Var1==1))
  }
activerate<-activerate[,-1]
names(activerate)<-c("Levels","ActiveRate","Covariate")
 
#plot the retention rates by groups 
ggplot(data=activerate, aes(x=Levels, y=ActiveRate*100))+
  geom_bar(stat="identity")+
  facet_wrap(~Covariate, ncol=2, scales="free_x")+
  xlab("Levels of Covariates")+
  ylab("Retention rate (%)")+
  ggtitle("Retention Rate by Covariates")+
  theme(strip.text.x = element_text(size=12))


#check variables dependency
smp_size <- floor(0.5 * nrow(df_num))
set.seed(123)
pairs(df_num)
        
#tree model to explore all covariates
library(rpart)


df2<-select(df, active, city ,trips_in_first_30_days, avg_rating_of_driver, avg_surge, phone, surgepct_grp,surge_pct, uber_black_user, weekday_pct, avg_dist,
            avg_rating_by_driver, numoftrips_grp,ratingbydriver_grp,ratingofdriver_grp,avgdist_grp,surgepct_grp,avg_surge_grp,wkdaypct_grp)

df2$active<-factor(df2$active)

#fit a rpart tree model 
fit <- rpart(as.factor(active) ~city + 
                                trips_in_first_30_days+ 
                                phone+
                                surge_pct+
                                uber_black_user+
                                weekday_pct+ 
                                avgdist_grp+
                                ratingbydriver_grp+ratingofdriver_grp,
             method="class", data=df2,cp=0.005,minsplit=100)
fancyRpartPlot(fit)


#Take a look at the driver rating = 5 cases
fit2 <- rpart(as.factor(active) ~city + 
               trips_in_first_30_days+ 
               phone+
               surge_pct+
               uber_black_user+
               weekday_pct+ 
               avg_dist+
              ratingofdriver_grp,
             method="class", data=filter(df2,avg_rating_by_driver==5),cp=0.005,minsplit=100)
fancyRpartPlot(fit2)

#random forest

smp_size <- floor(0.7 * nrow(df))
set.seed(123)
train_ind <- sample(seq_len(nrow(df2)), size = smp_size)
df_train<-df2[train_ind,]
df_test<-df2[-train_ind,]

trf = train(as.factor(active) ~city + 
              trips_in_first_30_days+ 
              phone+
              surge_pct+
              uber_black_user+avg_surge+
              weekday_pct+ 
              avg_dist+ratingofdriver_grp+ratingbydriver_grp
              , data=df2, method="rf",ntree=100,
            nodesize = 5, importance=TRUE, metric="Accuracy", 
            trControl = trainControl(method="oob"), 
            allowParallel=FALSE,do.trace=TRUE,
            subset = train_ind)
trf$results

df_test$pred_rf<-predict(trf, df_test)

tbl=table(df_test$pred_rf, df_test$active)
confusionMatrix(tbl)

#try logistic regression really quick

t_logistic = train(as.factor(active) ~city + 
              trips_in_first_30_days+ 
              phone+
              surge_pct+
              uber_black_user+
              weekday_pct+ 
              avg_dist+
              ratingofdriver_grp, data=df2, method="glm",
              subset = train_ind )
t_logistic$results

df_test$pred_logistic<-predict(t_logistic, df_test)
table(df_test$pred_logistic, df_test$active)
prop.table(table(df_test$pred_logistic, df_test$active))

