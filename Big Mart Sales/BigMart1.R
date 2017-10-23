#Exploration
# Item Identifier Food, Drinks, Non-consumables
# Item Weight has some NAs - impute to mean value of same id products 
# fat content separate into low fat and regular also remove for NCs 
# visibility 
# type and id highly correlated


library(ggplot2)
library(stringr)
library(graphics)
#library(dplyr)
#Read
train <- read.csv("Train.csv",header = TRUE)
test <- read.csv("Test.csv", header = TRUE)

#data.combined
test.Item_Outlet_Sales <- data.frame(Item_Outlet_Sales = rep("None", nrow(test)), test[,])
data.combined <- rbind(train,test.Item_Outlet_Sales)

summary(data.combined)
str(data.combined$Item_Identifier) 
length(unique(data.combined$Item_Identifier))
#extract first two letters from identifier

summary(data.combined$Item_Weight)
length(unique(data.combined$Item_Weight))

str(data.combined$Item_Fat_Content) #only two type low fat and reg
summary(data.combined$Item_Visibility) #outlier
summary(data.combined$Item_Type)
summary(data.combined$Item_MRP)
summary(data.combined$Outlet_Identifier)
summary(data.combined$Outlet_Establishment_Year)
summary(data.combined$Outlet_Size)
summary(data.combined$Outlet_Location_Type)
summary(data.combined$Outlet_Type)
summary(data.combined$Item_Outlet_Sales)
summary(train$Item_Outlet_Sales)

data.combined$Item_Outlet_Sales <-  as.numeric(data.combined$Item_Outlet_Sales)
FTCII<- NULL
Lastchar <- NULL
for(i in 1:nrow(data.combined))
{
  FTCII <- c(FTCII,substr(data.combined[i,"Item_Identifier"],1,2) )
  Lastchar <- c(Lastchar,substr(data.combined[i,"Item_Identifier"],3,9) )
}

data.combined$FTCII <- as.factor(FTCII)
data.combined$Lastchar <- as.factor(Lastchar)
summary(data.combined$FTCII)


index <- which(is.na(data.combined$Item_Weight))
index1 <- which(!is.na(data.combined$Item_Weight))

weight <- data.combined$Item_Weight[index1]
iid <- as.factor(data.combined$Item_Identifier[index1])
avg.weight <- aggregate(weight,by = list(iid), FUN = mean)
data.combined$Item_Weight[index] <- 
  avg.weight[which(data.combined$Item_Identifier[index] == avg.weight$Group.1),2]
length(unique(which(is.na(data.combined$Item_Weight))))

ggplot(data.combined, aes(x=Outlet_Size))+
  geom_bar() +
  facet_wrap(~Outlet_Location_Type + Outlet_Type)+
  xlab("Outlet Size") +
  ylab("Count")

index <- which(data.combined$Outlet_Size == '')
data.combined$Outlet_Size[index] <- 'Small'
index <- which(data.combined$Outlet_Size == '')

data.combined$Outlet_age <- 2013-data.combined$Outlet_Establishment_Year
Fat <- NULL

Fat[data.combined$Item_Fat_Content %in% c("Low Fat","low fat", "LF")] <- "LF"
Fat[data.combined$Item_Fat_Content %in% c("reg","Regular")] <- "Reg"
Fat[data.combined$FTCII =="NC"] <- "None"

data.combined$Fat <- as.factor(Fat)
str(data.combined$Fat)

index <- which(data.combined$Item_Visibility == 0)
index1 <- which(data.combined$Item_Visibility != 0)
visibility <- data.combined$Item_Visibility[index1]
iid <- as.factor(data.combined$Item_Identifier[index1])
avg.visibility <- aggregate(visibility, by =list(iid), FUN = mean)
data.combined$Item_Visibility[index] <-
  avg.visibility[which(data.combined$Item_Identifier[index] == avg.visibility$Group.1),2]
index <- which(data.combined$Item_Visibility == 0)

#seems like ftcii = item type
ggplot(data.combined[1:8523,], aes(x=FTCII, y = as.numeric(Item_Outlet_Sales)))+
  geom_boxplot() +
  xlab("FTCII") +
  ylab("Sales")


# ggplot(data.combined[1:8523,], aes(x=Lastchar, y = as.numeric(Item_Outlet_Sales)))+
#   geom_point(binwidth = 0.1) +
#   xlab("Lastchar") +
#   ylab("Sales")

#no dependence
ggplot(data.combined[1:8523,], aes(x= Item_Weight, y = Item_Outlet_Sales))+
  geom_point() +
  xlab("Weight") +
  ylab("Sales")

#influence
ggplot(data.combined[1:8523,], aes(x = Fat , y = Item_Outlet_Sales))+
  geom_boxplot() +
  xlab("Fat Content") +
  ylab("Sales")


#influential
ggplot(data.combined[1:8523,], aes(x = Item_Visibility , y = Item_Outlet_Sales))+
  geom_point() +
  xlab("Visibility") +
  ylab("Sales")

#Low influence
ggplot(data.combined[1:8523,], aes(x = Item_Type , y = Item_Outlet_Sales))+
  geom_boxplot() +
  xlab("Type") +
  ylab("Sales")

#Infuential
ggplot(data.combined[1:8523,], aes(x = Item_MRP , y = Item_Outlet_Sales))+
  geom_point() +
  xlab("MRP") +
  ylab("Sales")

ggplot(data.combined[1:8523,], aes(x = as.factor(Outlet_Establishment_Year) , y = Item_Outlet_Sales))+
  geom_boxplot() +
  xlab("Establishment Year") +
  ylab("Sales")


#low
ggplot(data.combined[1:8523,], aes(x = Outlet_Location_Type , y = Item_Outlet_Sales))+
  geom_boxplot() +
  xlab("Location Type") +
  ylab("Sales")

#influential
ggplot(data.combined[1:8523,], aes(x = Outlet_Type , y = Item_Outlet_Sales))+
  geom_boxplot() +
  xlab("Outlet Type") +
  ylab("Sales")

summary(data.combined$Item_Outlet_Sales[1:8523])

#low influence can check for size and type combined
ggplot(data.combined[1:8523,], aes(x = as.factor(Outlet_Size) , y = Item_Outlet_Sales))+
  geom_boxplot() +
  xlab("Size") +
  ylab("Sales")

ggplot(data.combined, aes(x = as.factor(Outlet_Size), y = Item_Outlet_Sales))+
  geom_point() +
  facet_wrap(~Outlet_Location_Type + Outlet_Type) +
  xlab("Size") +
  ylab("Count")  +
  ggtitle("Outlet Location Type")


#Categorical - outlet type, location type and outlet size and item type
#Continuous - MRP and Visibility

library(dummies)

var <- c("FTCII", "Fat", "Outlet_Location_Type","Outlet_Size","Outlet_Type")
dvar <- dummy.data.frame(data.combined, var, fun = as.numeric)

temp <- names(dvar) %in% c("Item_Identifier","Item_Fat_Content", "Item_Type", "Outlet_Identifier",
         "Outlet_Establishment_Year", "Lastchar")

dvar1 <- dvar[!temp]

temp1 <- names(dvar1) %in% c("Item_Outlet_Sales")
temp1 <- !temp1

library(glmnetUtils)

lrr <- glmnet(Item_Outlet_Sales ~ ., dvar1[1:8523,], alpha = 0)
plot(lrr,"dev")
print(lrr)


cvlrr <- cv.glmnet(Item_Outlet_Sales ~ ., dvar1, alpha = 0)
plot(cvlrr)

lrr.pred <- predict(cvlrr,dvar1[1:8523,])

library(ModelMetrics)
rmse(dvar1$Item_Outlet_Sales[1:8523],lrr.pred)

submission <- data.combined[8524:14204,c("Item_Identifier","Outlet_Identifier")]
test.pred <- predict(cvlrr,dvar1[8524:14204,])
submission$Item_Outlet_Sales <- test.pred

write.csv(submission,"submit.csv", row.names = FALSE)

rss <- sum((dvar1$Item_Outlet_Sales[1:8523] - lrr.pred)^2)
tss <- sum((dvar1$Item_Outlet_Sales[1:8523] - ave(dvar1$Item_Outlet_Sales[1:8523]))^2)
r2 <- 1 - (rss/tss)
rmse <- sqrt(rss/8523)
