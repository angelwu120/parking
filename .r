setwd("/Users/Angie/Desktop")
raw_pk <- read.csv("parking.csv",sep=";")
summary(raw_pk)
raw_pk$id <- NULL
raw_pk$a1 <- as.factor(raw_pk$a1)   # gender
raw_pk$a2 <- as.factor(raw_pk$a2)   # age
raw_pk$a3 <- as.factor(raw_pk$a3)   # income
raw_pk$a4 <- as.factor(raw_pk$a4)   # highest degree
raw_pk$a5 <- as.factor(raw_pk$a5)   # job position
raw_pk$a6 <- as.factor(raw_pk$a6)   # living area
raw_pk$a7 <- as.factor(raw_pk$a7)   # if owning a car
raw_pk$a8 <- as.factor(raw_pk$a8)   # if park available
raw_pk$a9 <- as.factor(raw_pk$a9)   # parking fee
raw_pk$b1 <- as.factor(raw_pk$b1)   # satisfaction with planning and construction
raw_pk$b2 <- as.factor(raw_pk$b2)   # satisfaction with the gov
raw_pk$b3 <- as.factor(raw_pk$b3)   # satisfaction with quantity
raw_pk$b4 <- as.factor(raw_pk$b4)   # satisfaction with location
raw_pk$b51 <- as.factor(raw_pk$b51) # far away from the destination
raw_pk$b52 <- as.factor(raw_pk$b52) # planning
raw_pk$b53 <- as.factor(raw_pk$b53) # no parking around the destination
raw_pk$b54 <- as.factor(raw_pk$b54) # others
raw_pk$b6 <- as.factor(raw_pk$b6)   # satisfaction with entry/exit
raw_pk$b7 <- as.factor(raw_pk$b7)   # satisfaction with system
raw_pk$b8 <- as.factor(raw_pk$b8)   # satisfaction with charge
raw_pk$b9 <- as.factor(raw_pk$b9)   # conmments
raw_pk$b10 <- as.factor(raw_pk$b10) # satisfaction with utilization
raw_pk$b11 <- as.factor(raw_pk$b11) # paking voilation
raw_pk$b12 <- as.factor(raw_pk$b12) # fine
raw_pk$b13 <- as.factor(raw_pk$b13) # distance
raw_pk$b14 <- as.factor(raw_pk$b14) # management
raw_pk$b15 <- as.factor(raw_pk$b15) # selecting parking lot
raw_pk$b16 <- as.factor(raw_pk$b16) # constrcting parking lot
raw_pk$b17 <- as.factor(raw_pk$b17) # parking timing
raw_pk$b18 <- as.factor(raw_pk$b18) # financing
raw_pk$b191 <- as.factor(raw_pk$b191) # shoping center
raw_pk$b192 <- as.factor(raw_pk$b192) # transition center
raw_pk$b193 <- as.factor(raw_pk$b193) # hospital
raw_pk$b194 <- as.factor(raw_pk$b194) # school
raw_pk$b195 <- as.factor(raw_pk$b195) # park
raw_pk$b196 <- as.factor(raw_pk$b196) # bar
raw_pk$b20 <- as.factor(raw_pk$b20)   # traffic

naomit_pk <- na.omit(raw_pk)


## Factor Analysis ##
library(psych)
raw_pk <- na.omit(raw_pk)
fa.parallel(raw_pk)
results<-factanal(raw_pk, 5, rotation="varimax")
loadings(results)


formula <- b1~b2+b3+b4+b6+b8+b9+b10+b12+b13+b18

pk1 <- naomit_pk[1:1000,]
pk2 <- naomit_pk[1001:1968,]

## Linear Regression ##
lm <- lm(formula,data=pk1)
summary(lm)
lr<-glm(formula,family=binomial,data=pk1)
pk2_lr<-pk2
pk2_lr$pred<-round(predict(lr,pk2_lr),digits=0)
table(pk2_lr$b1,pk2_lr$pred)

summary(lr)

## Random Forest ##
library(randomForest)
library(imputation)
library(MASS)
library(party)


dt<-ctree(formula,data=pk1)
pk2_dt<-pk2
pk2_dt$pred<-predict(dt,pk2)
table(pk2_dt$b1,pk2_dt$pred)
plot(pk2_dt$b1,pk2_dt$pred)
plot(dt, main="Conditional Inference Tree")

library(rpart)
rpart<-rpart(formula,method="class",data=pk1)
pk2_tree<-pk2
pk2_tree$pred<-predict(rpart,pk2)
# plot tree
plot(rpart, uniform=TRUE, main="Classification Tree for Chemicals")
text(rpart, use.n=TRUE, all=TRUE, cex=.8)



rf <- randomForest(formula, data=naomit_pk)
summary(rf)
rf<-randomForest(formula,data=pk1)
pk2_rf<-pk2
pk2_rf$pred<-predict(rf,pk2)
table(pk2_rf$b1,pk2_rf$pred)
plot(pk2_rf$b1,pk2_rf$pred)
plot(rf)


