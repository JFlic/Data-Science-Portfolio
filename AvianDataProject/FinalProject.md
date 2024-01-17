# Jack Flickinger
# Final Project
# Data Science


##
##
# Part 1
##
##
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r Model }
library(stats)
library(ggplot2)
library(factoextra)
library(corrplot)
library(cluster)
```

AvianMeasurements = read.csv("AvianMeasurements.csv")

set.seed(3931134)

head(AvianMeasurements)
str(AvianMeasurements)
summary(AvianMeasurements)


###Clean data
AvianMeasurements = AvianMeasurements[complete.cases(AvianMeasurements),]
str(AvianMeasurements)

##### Determining the Best Value of k using Elbow Plots

corrplot(cor(AvianMeasurements))
corrplot(cor(AvianMeasurements), method="number", type = "lower")

df <- scale(AvianMeasurements)

### Optimum Cluster: Euclidean Distance
set.seed(123)

Birdr <- sample(1:342, 10)
Birdr1 <- df[Birdr,]
head(Birdr1)

distE <- dist(Birdr1, method='euclidean')
head(distE)

### Heat Map
fviz_dist(distE)


##### Determining the Best Value of k using Elbow Plots

wss <- sapply(1:Birdr, function(k){kmeans(AvianMeasurements, k, nstart=20, iter.max=15)$tot.withinss})
plot(1:Birdr, wss, type="b", pch =19, frame=FALSE, xlab="Number of Clusters K", ylab="Total within-clusters sum of squares")

#shows which amount of cluster makes biggest difference
fviz_nbclust(AvianMeasurements, kmeans, method="wss") + geom_vline(xintercept =3, linetype= 5, col= "darkred")
### K-Means with k=3

### Output of Kmeans

# Lets go by when the fviz_nbclust tells us first
km.res <- kmeans(df, 3, nstart = 25)
km.res

km.res$totss
km.res$betweenss

km.res$betweenss/km.res$totss

#### Including CLusters in USArrests

df_member <- cbind(AvianMeasurements, cluster = km.res$cluster)
head(df_member)

fviz_cluster(km.res, data=AvianMeasurements)

fviz_cluster(km.res, data = AvianMeasurements,
             palette=c("red", "blue", "black", "darkgreen"),
             ellipse.type = "euclid",
             star.plot = T,
             repel = T,
             ggtheme = theme())

km.res <- kmeans(df, 4, nstart = 25)

fviz_cluster(km.res, data = AvianMeasurements,
             palette=c("red", "blue", "black", "darkgreen"),
             ellipse.type = "euclid",
             star.plot = T,
             repel = T,
             ggtheme = theme())

km.res <- kmeans(df, 2, nstart = 25)

fviz_cluster(km.res, data = AvianMeasurements,
             palette=c("red", "blue", "black", "darkgreen"),
             ellipse.type = "euclid",
             star.plot = T,
             repel = T,
             ggtheme = theme())

aggregate(AvianMeasurements, by=list(cluster=df_member$cluster), mean)

aggregate(AvianMeasurements, by=list(cluster=df_member$cluster), median)

aggregate(AvianMeasurements, by=list(cluster=df_member$cluster), range)

# I think that the best k would be k = 3 because visually looking at the plot
# there seems to be 3 distint groups of birds and anything more than that 

####
####
#Use Hierarchical clustering to group the AvianMeasurements dataset.
####
####

library(tidyverse)

# I decided to use a k = 3 in the dendrograms because it was shown in the clustering
# That it was the best choice.

#2.
#Produce a dendrogram using AGNES
res.agnes <- agnes(x=AvianMeasurements, stand = TRUE, 
                   metric = "euclidean", method="ward")

#2a.
fviz_dend(res.agnes, cex=0.6, k=3)

# Find the dis(similarity)
res.dist <- dist(df, method="euclidean")
as.matrix(res.dist)[1:6, 1:6]   #to see an output of the data

### Conduct the HC with the hclust() and the ward linkage method
res.hc <- hclust(d=res.dist, method="ward.D2")

#2b.
### Finding the cophenetic distances (height)
res.coph <- cophenetic(res.hc)

### Comparing cophenetic distances with original distances
cor(res.dist, res.coph)

#### Comparing to the linkage method average
res.hc2 <- hclust(res.dist, method = "average")
cor(res.dist, cophenetic(res.hc2))

#2c.
# It looks to me like again there are 3 distinct groups that pretain to
# the Avian Dendrogram same as the clustering plot. 
fviz_dend(res.hc, cex=0.5)

#### Identifying groups
head(grp)
rownames(df)[grp== 2]

#2d.
#### Plotting using color
fviz_dend(res.hc, k=3, cex =0.5, 
          k_colors =c("#2E9FDF", "#00AFBB", "#E7B800" , "red","green"), 
          color_labels_by_k = TRUE, rect=TRUE)

### Visualizing results as a scatter plot
fviz_cluster(list(data= df, cluster=grp), 
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "red", "green"),
             ellipse.type= "convex", repel=TRUE, show.clust.cent = FALSE, 
             ggtheme=theme_minimal())


#### Streamlining the processes: The Cluster package

res.agnes <- agnes(x=AvianMeasurements, stand = TRUE, 
                   metric = "euclidean", method="ward")

res.diana <- diana(x=AvianMeasurements, stand = TRUE, 
                   metric = "euclidean")



fviz_dend(res.agnes, cex=0.6, k=3)
# AGNES has a lot branches towards the bottom but three main branches
# that those branchs from the bottom come from.

# AGNES shows that there are basically three main groups that every bird in the data set
# can go into.

fviz_dend(res.diana, cex=0.6, k=3)
# DIANA has a much more branches towards the top. 

# DIANA shows that the green group is actually a very small group
# that has a slight similarity to the red group

# DIANA shows a much more specific subgroups of the main three groups


#4. I think that the most satisfying model was the clustering model just
# because it looked a lot cleaner with all the data points rather than the 
# deprograms.

##
##
# Part 2
##
##

library(tidyverse)
library(ipred)
library(rpart)
library(adabag)
library(caTools)
set.seed(123)

#1 Divide the data into a test a train set (70/30).

abalone = read.csv("abalone.csv")
summary(abalone)
str(abalone)
names(abalone) <- c("Sex", "Length", "Diameter", "Height", "Wholeweight", "Shuckedweight", "Visceraweight", "Shellweight", "rings")

abalone$Sex <- as.factor(abalone$Sex)
class(abalone$Sex)


split <- sample.split(abalone$Sex, SplitRatio = 0.7)
split
train_data <- abalone[split, ]  
str(train_data)
test_data <- abalone[!split, ]

train_data$Sex <- as.factor(train_data$Sex)

test_data$Sex <- as.factor(test_data$Sex)

#2.
################# Bagging ####################################

# Install packages
library(rpart)
install.packages("adabag")
library(adabag)

formula <- Sex ~ .
vardep <- abalone[, as.character(formula[[2]])]
cntrl <-rpart.control(maxdepth=2, minsplit = 1, cp = -1)

abalone.bagging <- bagging(formula = formula, 
                           data = train,
                           mfinal = 10,
                           control = cntrl)
abalone.bagging

abalone.bagging$samples[,1]
summary(as.factor(abalone.bagging$samples[,1]))
importanceplot(abalone.bagging, horiz=FALSE)
abalone.bagging$importance

#Training
table(abalone.bagging$class, abalone$Sex[train_data$Sex], dnn=c("Predicted class", "Observed class"))

#Test set
abalone.prebagging <- predict.bagging(abalone.bagging, newdata =abalone[-train$Sex,])
abalone.prebagging

abalone.baggingcv <- bagging.cv(Sex ~. , v=10, data=abalone, mfinal=10,
                             control=rpart.control(maxdepth=1))
abalone.baggingcv

#3.
################# Boosting ####################################


abalone.adaboost <- boosting(formula = formula, 
                           data = train,
                           mfinal = 10,
                           control = cntrl)
abalone.adaboost

importanceplot(abalone.adaboost, horiz=FALSE)

# Training Error
table(abalone.adaboost$class, abalone$Sex[train$Sex], dnn=c("Predicted Class", "Observed Class"))
1-sum(abalone.adaboost$class == abalone$Sex[train$Sex])/length(abalone$Sex[train$Sex])

#Test error

abalone.predboosting <- predict.boosting(abalone.adaboost, newdata = abalone[-train$Sex,])
abalone.predboosting

### Boosting Error by cv

abalone.boostcv <- boosting.cv(Sex~., v=10, data=abalone, mfinal=10,
                            control = rpart.control(maxdepth=1))
abalone.boostcv

#4.
################# Random Forest ####################################

library(tidyverse) 

data <-  abalone %>% 
  mutate(set = ifelse(runif(nrow(.)) > 0.70, "test", "train"))
train <- data %>% filter(set == "train") %>% select(-set)
test <- data %>% filter(set == "test") %>% select(-set)

# Quick inspection of the data set
glimpse(data)

##### Measure the prediction error
rmse <- function(x) sqrt(sum((x - test$Sex)^2))


##################### Random Forest ####################################
### There is a library called randomForest but ranger is faster #########
library(ranger)

# only a tree
first_rf <- ranger(Sex ~ Length + Diameter + Height + Wholeweight
                   + Shuckedweight + Visceraweight + Shellweight 
                   + rings , 
                   num.trees = 1, mtry = 5, data = train)
first_rf


# Make some predictons using the test set

pred_rf_first <- predict(first_rf, test)$predictions

rmse(pred_rf_first)