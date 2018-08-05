getwd()

iris_dataset = read.csv("iris_Dataset/iris.csv", stringsAsFactors = F)

View(iris_dataset)

print(head(iris_dataset, 3)) # A. Explore / Print first 3 Records from Dataset.
print(dim(iris_dataset)) # B. Find Dimension of Dataset.

# C. Find Names , Class of features in the Dataset.
print(names(iris_dataset))

class(iris_dataset)

str(iris_dataset)

class(iris_dataset$sepal_length)

class(iris_dataset$sepal_width)
class(iris_dataset$petal_length)
class(iris_dataset$petal_width)
class(iris_dataset$species)

# Find missing values (if any) & make the data consistent by removing it.
is.na(iris_dataset)
iris_dataset <- na.omit(iris_dataset)

# E. Find Structure of Data.
str(iris_dataset)

# F. Find mean, median, quartile, max, min data for every feature.
summary(iris_dataset)

# G. Plot a Boxplot Graph, Pie chart respective to their Species.
pie(table(iris_dataset$species), col = c("blue","red","green"), main = "Classification of iris species", radius= 1)

boxplot(iris_dataset$sepal_length~iris_dataset$species, xlab = "Species", ylab = "Sepal Length", names.args = iris_dataset$species, col = "blue")

boxplot(iris_dataset$sepal_width~iris_dataset$species, xlab = "Species", ylab = "Sepal width", names.args = iris_dataset$species, col = "blue")

boxplot(iris_dataset$petal_length~iris_dataset$species, xlab = "Species", ylab = "Petal Length", names.args = iris_dataset$species, col = "blue")

boxplot(iris_dataset$petal_width~iris_dataset$species, xlab = "Species", ylab = "Petal Width", names.args = iris_dataset$species, col = "blue")

# H. Subset tuples based on their Species in different R-Object.
iris_versicolor <- subset(iris_dataset, iris_dataset$species == "versicolor")
print(class(iris_versicolor))

iris_setosa <- subset(iris_dataset, iris_dataset$species == "setosa")
print(iris_setosa)

iris_virginica <- subset(iris_dataset, iris_dataset$species == "virginica")
print(iris_virginica)

# I. Plot a BoxPlot Graph for Individual R-Object.

boxplot(iris_dataset$sepal_length)
boxplot(iris_dataset$sepal_width)
boxplot(iris_dataset$petal_length)
boxplot(iris_dataset$petal_width)

# J. Plot a Histogram on feature Petal lengths of iris dataset .
hist(iris_dataset$petal_length, col = "red", xlab = "Petal Length", main = "Histogram of Petal Length of Iris Data", ylim = c(0,50))

# K. Plot a Histogram for Petal Lengths of Different Species on different Graph.

hist(iris_versicolor$petal_length, xlab = "Petal Length", col = "red", main = "Histogram for Petal Lengths of Versicolor")

hist(iris_setosa$petal_length, xlab = "Petal Length", col = "blue", main = "Histogram for Petal Lengths of Setosa")

hist(iris_virginica$petal_length, xlab = "Petal Length", col = "green", main = "Histogram for Petal Lengths of virginica")

# L. Find correlation between multiple features also plot a scatter plot for correlation.

cor(iris_dataset$sepal_length, iris_dataset$sepal_width)

plot(x = iris_dataset$sepal_length, y = iris_dataset$sepal_width,
     xlab = "Sepal Length", ylab = "Sepal Width",
     main = "Sepal Length VS Sepal Width")

cor(iris_dataset$petal_length, iris_dataset$petal_width)

plot(x = iris_dataset$petal_length, y = iris_dataset$petal_width,
     xlab = "Petal Length", ylab = "Petal Width",
     main = "Petal Length VS Petal Width")

cor(iris_dataset$sepal_length, iris_dataset$petal_length)

plot(x = iris_dataset$sepal_length, y = iris_dataset$petal_length,
     xlab = "Sepal Length", ylab = "Petal Length",
     main = "Sepal Length VS Petal Length")

cor(iris_dataset$sepal_width, iris_dataset$petal_width)

plot(x = iris_dataset$sepal_width, y = iris_dataset$petal_width,
     xlab = "Sepal Width", ylab = "Petal Width",
     main = "Sepal Width VS Petal Width")

cor(iris_dataset$sepal_length, iris_dataset$petal_width)

plot(x = iris_dataset$sepal_length, y = iris_dataset$petal_width,
     xlab = "Sepal Length", ylab = "Petal Width",
     main = "Sepal Length VS Petal Length")

cor(iris_dataset$sepal_width, iris_dataset$petal_length)

plot(x = iris_dataset$sepal_width, y = iris_dataset$petal_length,
     xlab = "Sepal Width", ylab = "Petal Length", 
     main = "Sepal Width VS Petal Length")


# 4. Classify Data based on iris Species and plot a Decision Tree.

library(rpart)
library(rpart.plot)
install.packages("rattle")
library(rattle)

classify1 <- rpart(iris_dataset$species ~ . , data = iris , method = 'class')

fancyRpartPlot(classify1 , main = ' Classification of Iris dataset' )
