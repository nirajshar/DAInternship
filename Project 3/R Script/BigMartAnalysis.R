# 1. Import Train & Test DataSet from BigMart Dataset folder

big_mart_train <- read.csv("BigMart Sales Dataset/Train_UWu5bXk.csv", stringsAsFactors = FALSE)

big_mart_test <- read.csv("BigMart Sales Dataset/Test_u94Q5KV.csv", stringsAsFactors = FALSE)

View(big_mart_train)
View(big_mart_test)

# 2. Check dimensions (number of row & columns) & Structure in dataset.

dim(big_mart_train)
dim(big_mart_test)

str(big_mart_train)
str(big_mart_test)

# 3. Find Missing Values in the dataset.

table(is.na(big_mart_train))
table(is.na(big_mart_test))

# 4. Find Missing Values according to Columns.

colSums(is.na(big_mart_train))
colSums(is.na(big_mart_test))

# 5. Find Summary of DataSet & Draw Conclusions from it.

summary(big_mart_train)
summary(big_mart_test)

# 6. ScatterPlots
# A. Plot a ScatterPlot using ggplot for Item_Visibility vs Item_Outlet_Sales & draw conclusion from which products visibility is more sales.

install.packages("ggplot2")
library(ggplot2)

ggplot()+
  geom_point(aes(x = big_mart_train$Item_Visibility, y = big_mart_train$Item_Outlet_Sales), colour = 'red')+
  ggtitle('Visibility VS Outlet Sales')+
  xlab('Visibilty')+
  ylab('Outlet Sales')

visi_vs_sales <- recordPlot()

visi_vs_sales

# B. Plot a Barplot using ggplot for Outlet_Identifier vs Item_Outlet_Sales & Draw conclusion who has contributed to majority of sales.

library(scales)

ggbarplot <- ggplot(big_mart_train, aes(big_mart_train$Outlet_Identifier, big_mart_train$Item_Outlet_Sales))+ geom_bar(stat = 'identity', colour = 'light green')+
  scale_y_continuous(labels = comma)+
  labs(x="Outlet Identifier",y="Item Outlet Sales")

ggbarplot

# C. Plot a Barplot using ggplot for Item_Type vs Item_Outlet_Sales also draw conclusion which items are sold more.

ggbarplot2 <- ggplot(big_mart_train, aes(big_mart_train$Item_Type, big_mart_train$Item_Outlet_Sales))+
  geom_bar(stat = 'identity', colour = 'turquoise')+
  scale_y_continuous(labels = comma)+
  labs(x = 'Item Type', y = 'Item Outlet Sales')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggbarplot2

# D. Plot a Boxplot using ggplot for Item_Type vs Item_Outlet_Sales also draw conclusion which items are sold more.

ggboxplot <- ggplot(big_mart_train, aes(big_mart_train$Item_Type, big_mart_train$Item_MRP))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = 'Item Type', y = 'Item MRP')

ggboxplot

# 7. Manipulating Dataset to make it consistent
# A. Add Item_Outlet_Sales Column to test dataset which is'nt available & assign integer 1. Also Combine Both Train + Test Datasets.

big_mart_test$Item_Outlet_Sales <- 1
big_mart_sales <- rbind(big_mart_train, big_mart_test)
dim(big_mart_sales)

# B. Impute missing value in Item_Weight using median because it is highly robust to Outliers.

big_mart_sales$Item_Weight[is.na(big_mart_sales$Item_Weight)] <- median(big_mart_sales$Item_Weight, na.rm = TRUE)

View(big_mart_sales)

# C. We saw item visibility has zero value also, which is practically not feasible. Impute median value where item_visibility 0.

big_mart_sales$Item_Visibility <- ifelse(big_mart_sales$Item_Visibility == 0, median(big_mart_sales$Item_Visibility), big_mart_sales$Item_Visibility)

View(big_mart_sales)

# D. Rename level in Outlet_Size to since mis-matched levels in variables needs to be corrected.

big_mart_sales$Outlet_Size <- as.factor(big_mart_sales$Outlet_Size)
nlevels(big_mart_sales$Outlet_Size)

big_mart_sales$Outlet_Size

levels(big_mart_sales$Outlet_Size)[1] <- "Other"

# E. Rename levels of Item_Fat_Content since value are "LF" / "low fat", so make them consistent.

library(plyr)
library(dplyr)

big_mart_sales$Item_Fat_Content <- revalue(big_mart_sales$Item_Fat_Content, c("LF" = "Low Fat", "reg" = "Regular", "low fat" = "Low Fat"))

# F. Create a new column 2013 - Year ( For Prediction ).

big_mart_sales$Year <- 2013 - big_mart_sales$Outlet_Establishment_Year

# G. Drop variables not required in modelling i.e. Item_Identifier, Outlet_Identifier, Outlet_Establishment_Year as they aren't needed for prediction.

big_mart_sales <- select(big_mart_sales, -c(Item_Identifier, Outlet_Establishment_Year, Outlet_Identifier))

# H. Divide data set into Train and Test.
# new_train <- df[1:nrow(train),]

big_mart_train_new <- big_mart_sales[1:nrow(big_mart_train),]
big_mart_test_new <- big_mart_sales[-(1:nrow(big_mart_train)),]
new_test = big_mart_test_new
View(big_mart_test_new)
# I. Perform a Regression testing on training dataset

relation <- lm(big_mart_train_new$Item_Outlet_Sales~., data = big_mart_train_new)

# J. Plot Summary and Predict sales for Testing Dataset.

y_predicted <- predict(relation, new_test)

summary(relation)
View(y_predicted)
