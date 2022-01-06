#"Bike Sharing Demand Kaggle challenge" Project

bike_share <- read.csv('Bikeshare_Data.csv')
head(bike_share)

#Exploratroy Data Analysis
#Create a scatter plot of count Vs temp
library(ggplot2)
ggplot(bike_share, aes(temp, count)) + geom_point(alpha=0.5, aes(color=temp)) + theme_bw()

#Plot count versus datetime as scatterplot with color gradient based on temp
#To do this, we need to convert datetime column into POSIXct
bike_share$datetime <- as.POSIXct(bike_share$datetime)
ggplot(bike_share, aes(datetime, count)) + geom_point(aes(color=temp),alpha=0.5) + scale_color_continuous(low='#55D8CE',high = '#FF6E2E') + theme_bw()

#We have noticed seasonality in the data(for winter and summer) and also rental counts are increasing. This can cause problem using linear regression model if data is non-linear.Despite that,
#Let us find correlation between temp and count
cor(bike_share[, c('temp','count')])

#To explore the season data, let us create a boxplot, count vs season
ggplot(bike_share,aes(factor(season),count)) + geom_boxplot(aes(color=factor(season))) + theme_bw()

#As there are more rentals in winter than in spring and a line can't capture non-linear relation
#Let us create "hour" column that takes the hour from the datetime column to seek further insight
bike_share$hour <- sapply(bike_share$datetime, function(x){format(x,"%H")})
head(bike_share)

#Let us create a scatterplot of count versus hour with color scale based on temp where workingday==1
library(dplyr)
pl <- ggplot(filter(bike_share, workingday==1), aes(hour,count))
pl <- pl + geom_point(position = position_jitter(w=1, h=0), aes(color=temp),alpha=0.5)
pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl + theme_bw()

#Create same plot for non-workingday i.e.workingday==0
pl1 <- ggplot(filter(bike_share, workingday==0), aes(hour,count))
pl1 <- pl1 + geom_point(position = position_jitter(w=1, h=0), aes(color=temp),alpha=0.5)
pl1 <- pl1 + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl1 + theme_bw()

#We have noticed that working days have peaked activity during morning (~8 am) and drops after peak working hours (~5-6 pm) with some lunchtime activity. In non-work days, activity starts picking after 8 am, peaks between 12am-4pm and starts falling thereafter

#Now, we will build a model by looking at this single feature(temp)
temp.model <- lm(count~temp, bike_share)
summary(temp.model)

#We got intercept of 6.0462 and temp 9.1705 as coeffecient, we predict number of bikes at certain temperature, e.g.calculate number of bikes at 25 degree C temp
6.0462 + 9.1705*25
#OR (method2)
temp.test <- data.frame(temp=c(25))
predict(temp.model,temp.test)

#Change the hour column of numberic values with use of sapply() and as.numberic
bike_share$hour <- sapply(bike_share$hour, as.numeric)

# Split up the sample
library(caTools)
sample <- sample.split(bike_share$temp, SplitRatio = 0.80) # SplitRatio = percent of sample==TRUE

# Training Data
train = subset(bike_share, sample == TRUE)

# Testing Data
test = subset(bike_share, sample == FALSE)

#Build a model(train) that attempt to predict based on the features we chose from the dataset.
model <- lm(count ~ . -casual - registered -datetime -atemp,bike_share)
summary(model)

#Visualize the model
#find residuals
res <- residuals(model)
res <- as.data.frame(res)
head(res)

#Histogram of residuals
ggplot(res, aes(res)) + geom_histogram(fill='blue',alpha=0.5)

#Plot the model
plot(model)

#Predict the testing data
count.predictions <- predict(model,test)

#to get predicted values
results <- cbind(count.predictions,test$count)
colnames(results) <- c('pred','real')
results <- as.data.frame(results)

#mean squared error
mse <- mean((results$real-results$pred)^2)
mse

#Root mean squared error
mse^0.5   

#To calculate R-squared for the model
SSE = sum((results$pred - results$real)^2)
SST = sum( (mean(bike_share$count) - results$real)^2)

R2 = 1 - SSE/SST
R2