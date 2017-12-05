#Load Libraries
library(caret)
library(dplyr)
library(caTools)
library(ggplot2)
library(e1071)
library(rpart)
library(rpart.plot)
library(randomForest)
library(cluster)


#Load data
train_sales<-read.csv("train.csv")

test_sales<-read.csv("test.csv")

View(train_sales)
View(test_sales)
test_sales$Item_Outlet_Sales<- "NA"

colSums(train_sales=="")
colSums(is.na(train_sales))

colSums(test_sales=="")
colSums(is.na(test_sales))

total_sales<-rbind(train_sales,test_sales)
colSums(total_sales=="")
colSums(is.na(total_sales))


total_sales<- droplevels(total_sales)

#Data Exploration
levels(total_sales$Item_Fat_Content)

total_sales$Item_Fat_Content[total_sales$Item_Fat_Content=="LF"]<-"Low Fat"
total_sales$Item_Fat_Content[total_sales$Item_Fat_Content=="low fat"]<-"Low Fat"
total_sales$Item_Fat_Content[total_sales$Item_Fat_Content=="reg"]<-"Regular"

total_sales$Item_Fat_Content<-droplevels(total_sales$Item_Fat_Content)

# Feature Scaling
#total_sales[4] = scale(total_sales[4])


# 4016 entry of outlet size is blank

ggplot(total_sales, aes(x=Outlet_Size, fill=factor(Outlet_Type)))+
  geom_bar(aes(fill=factor(Outlet_Type)))

# empty values of grocery Store type will be small
total_sales$Outlet_Size[total_sales$Outlet_Size==""& total_sales$Outlet_Type=="Grocery Store"]<-"Small"



ggplot(total_sales, aes(x=Outlet_Size, fill=factor(Outlet_Location_Type)))+
  geom_bar(aes(fill=factor(Outlet_Location_Type)))

# all blanks are of tier2 type
total_sales$Outlet_Size[total_sales$Outlet_Size==""& total_sales$Outlet_Location_Type=="Tier 2"]<-"Small"


total_sales$Outlet_Size<-droplevels(total_sales$Outlet_Size)


#2439 entries of Item weight is blank
median<-median(total_sales$Item_Weight[!is.na(total_sales$Item_Weight)])

total_sales$Item_Weight[is.na(total_sales$Item_Weight)]<-median

total_sales$Item_Outlet_Sales<- as.numeric(total_sales$Item_Outlet_Sales)


aov(total_sales[1:8523,]$Item_Outlet_Sales ~ total_sales[1:8523,]$Item_MRP)
aov(total_sales[1:8523,]$Item_Outlet_Sales ~ total_sales[1:8523,]$Outlet_Type +total_sales[1:8523,]$Item_MRP)
#Prective modeling1
#Regression
set.seed(123)
regressor<- lm(formula=Item_Outlet_Sales~ Outlet_Size +Outlet_Location_Type+Outlet_Type+Item_MRP , data= total_sales[1:8523,-1])

train_pred<-predict(regressor, newdata=total_sales[1:8523,-12])


test_pred<- predict(regressor, newdata=total_sales[8524:14204,-12])

ggplot() +
  geom_point(aes(x = total_sales[1:852,6], y = total_sales[1:852,12]),
             colour = 'red') +
  geom_line(aes(x = total_sales[1:852,6], y = train_pred[1:852]),
            colour = 'blue') 


ggplot() +
  geom_point(aes(x = total_sales[1:8523,6], y = total_sales[1:8523,12]),
             colour = 'red') +
  geom_line(aes(x = total_sales[8524:14204,6], y = test_pred),
            colour = 'blue') 


ggplot()+
  geom_line(aes(x=factor(total_sales[1:8523,]$Outlet_Type),y=total_sales[1:8523,]$Item_Outlet_Sales),colour='red')+
  geom_point(aes(x=factor(total_sales[8524:14204,]$Outlet_Type),y=test_pred),colour='blue')
  


ggplot(total_sales[1:8523,],
       aes(x = Item_Outlet_Sales,  fill = Item_MRP, group = Item_MRP)) +
  geom_bar()


#SVM
total_sales_sub<-total_sales[,6:12]
total_sales_sub<-total_sales_sub[,-2:-3]
regressor_svm = svm(formula = Item_Outlet_Sales ~ .,
                data = total_sales_sub[1:8523,],
                type = 'eps-regression',
                kernel = 'radial')


# Predicting a new result
y_pred_svm = predict(regressor_svm, total_sales_sub[8524:14204,-5])


ggplot() +
  geom_point(aes(x = total_sales_sub[1:8523,]$Item_MRP, y = total_sales_sub[1:8523,]$Item_Outlet_Sales),
             colour = 'red') +
  geom_line(aes(x = total_sales_sub[8524:14204,]$Item_MRP, y = y_pred_svm),
            colour = 'blue') +
  ggtitle(' (SVR)') +
  xlab('Item Mrp') +
  ylab('Item Outlet Sales')

ggplot() +
  geom_point(aes(x = factor(total_sales_sub[1:8523,]$Outlet_Type), y = total_sales_sub[1:8523,]$Item_Outlet_Sales),
             colour = 'yellow') +
  geom_line(aes(x = factor(total_sales_sub[8524:14204,]$Outlet_Type), y = y_pred_svm),
            colour = 'blue') +
  ggtitle(' (SVR)') +
  xlab('Outlet Type') +
  ylab('Item Outlet Sales')



#Decision Tree
reg_dt<-rpart(formula = Item_Outlet_Sales ~ .,
              data = total_sales_sub[1:8523,],
              control = rpart.control(minsplit = 10))

# Predicting a new result
y_pred_dt = predict(reg_dt, total_sales_sub[8524:14204,-5])

# Plotting the tree
par( mfrow = c( 1,1 ) )
rpart.plot(reg_dt)


#Random Forest
reg_rf<-randomForest(formula = Item_Outlet_Sales ~ .,
                     data = total_sales_sub[1:8523,],
                     ntree=500)
y_pred_rf<-predict(reg_rf, total_sales_sub[8524:14204,-5])


ggplot() +
  geom_point(aes(x = total_sales_sub[1:8523,]$Item_MRP, y = total_sales_sub[1:8523,]$Item_Outlet_Sales),
             colour = 'red') +
  geom_line(aes(x = total_sales_sub[8524:14204,]$Item_MRP, y = y_pred_rf),
            colour = 'blue') +
  ggtitle(' (RF)') +
  xlab('Item Mrp') +
  ylab('Item Outlet Sales')

ggplot() +
  geom_point(aes(x = factor(total_sales_sub[1:8523,]$Outlet_Type), y = total_sales_sub[1:8523,]$Item_Outlet_Sales),
             colour = 'yellow') +
  geom_line(aes(x = factor(total_sales_sub[8524:14204,]$Outlet_Type), y = y_pred_rf),
            colour = 'blue') +
  ggtitle(' (RF)') +
  xlab('Outlet Type') +
  ylab('Item Outlet Sales')



# svm gives the best result
total_sales_sub[8524:14204,5]<-y_pred_svm
total_sales[8524:14204,12]<-y_pred_svm


solution <- data.frame('Item_Identifier' = total_sales[8524:14204,]$Item_Identifier, 'Outlet_Identifier'=total_sales[8524:14204,]$Outlet_Identifier,'Item_Outlet_Sales'=total_sales[8524:14204,]$Item_Outlet_Sales)

head(solution)

# Write it to file
write.csv(solution, 'D:/Data Science/Analytics Vidya/Practice_projects/Big Mart Sales/Submission.csv', row.names = F)


#K means clustering
total_sales_otype<-total_sales_sub[,4:5]
total_sales_mrp<-total_sales_sub[c("Item_MRP", "Item_Outlet_Sales")]


# Fitting K-Means for Item Mrp
set.seed(29)
kmeans_mrp<- kmeans(x=total_sales_mrp, centers = 2)

# Visualising the clusters
clusplot(total_sales_mrp[1:500,],
         kmeans_mrp$cluster[1:500],
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Item Mrp',
         ylab = 'Item Outlet sales')
