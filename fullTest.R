library(nycflights13)
library(tidyverse)
library(plyr)
library(glmnet)

data(flights)
data(weather)
data(airports)
data(airlines)
data(planes)

set.seed(1212)

# Flights with all NAs removed
flights = flights[which(!is.na(flights[15])),]

# Flights and planes combined -- NAs no longer removed!
flightsP = left_join(flights, planes, by="tailnum")

# Adds binary variable that is 1 if there is a significant delay (>= 15 minutes)
flightsP$sigDelay = ifelse(flightsP$dep_delay >= 15, 1, 0)

carrierNames = names(table(flightsP$carrier)[order(table
                                  (flightsP$carrier),decreasing=T)][1:9])

# Leaves only top 9 carriers, which account for 97% of the data
flightsP = flightsP[which(flightsP$carrier %in% carrierNames),]

# Fix character variables to factor. I don't know if this is necessary or
# if I solved whatever problem I thought this caused in a different way, but 
# it's instant
flightsP <- flightsP %>% mutate_if(is.character, as.factor)

# Fix NAs in flightsP
#view(flightsP)
sum(is.na(flightsP)) # 657477, that's a lot
# Many have 8 columns with all NAs, we can try to remove those rows
flightsP = flightsP[which(!is.na(flightsP[22])),]
# Also, speed column basically all NA, remove it
flightsP = flightsP %>% select(-"speed")
# All the rest are in year. At this point we have actually removed a large
# amount of data, and it would be a shame to get rid of even more. I'll do it
# for now just to get something working, but IDK what to do for the future.
flightsP = flightsP[which(!is.na(flightsP[20])),]


# Removing variables that I know we won't want. 1 is year, which is always
# 2013, 19 is time_hour which is redundant, and the rest are things that can't
# be known until after a delay has occurred, like arrival delay or arrival time,
# therefore they wouldn't be able to help predict delays.
flightSmall = flightsP[-c(1,4,6,7,8,9,15,19)]

flightSmall = flightSmall[sample(nrow(flightSmall), 210972),]




boxplot(flightsP$dep_delay[which(flightsP$dep_delay > 20)]~
          flightsP$origin[which(flightsP$dep_delay > 20)], outline=F, 
        xlab = "Origin Airport", ylab="Length of Delay (Minutes)",
        main="Length of Delay by Origin Airport")

boxplot(flights2$dep_delay[which(flights2$dep_delay > 200 & 
                                   flights2$dep_delay > 0)]~
          flights2$origin[which(flights2$dep_delay > 200 & 
                                  flights2$dep_delay > 0)])



# This first test is using sigDelay and a binary response.

# It seems that using tailnum is too big, I literally can't even make the 
# matrix with just tailnum and nothing else. However, maybe the reason it
# was performing so well before is because with a small number of flights
# every tailnum only had a handful of flights, so knowing the tailnum basically
# tells you whether the flight was delayed or not...in which case, it wouldn't
# be as necessary/useful
x = model.matrix(sigDelay~.,flightSmall)[,-1]
y = flightSmall$sigDelay

# LASSO
lasso.cv = cv.glmnet(x,y,alpha=1)
plot(lasso.cv)  
lambda.cv = lasso.cv$lambda.min
lambda.cv # Lambda for 80% of data and using tailnum is 0.001676179 for future
          # reference. Since it took 24 minutes to run

fit.lasso = glmnet(x,y,alpha=1,lambda=lambda.cv)

CF = as.matrix(coef(fit.lasso, fit.lasso$lambda.cv))
CF[CF!=0,][order(CF[CF!=0])]

# We see different variables important now...model seems like the most, then 
# destination, manufacturer, carrier, then day veeeery far behind, then 
# sched_depart_time, hour, minute, year.y (plane built year)

# model thru carrier have coeffs in the 0.1-0.2 range. The rest
# are 0.0001-0.0002,






# Going to retry this with only top 20 models and 33 destination (models or 
# dests with over 1% of flights)
modelNames = names((table(flightSmall2$model)/nrow(flightSmall2))[order(
  table(flightSmall2$model)/nrow(flightSmall2),decreasing=T)][1:20])

destNames = names((table(flightSmall2$dest)/nrow(flightSmall2))[order(
  table(flightSmall2$dest)/nrow(flightSmall2),decreasing=T)][1:33])


flightSmall2 = flightsP[-c(1,4,7,8,9,12,15,19,27)]

flightSmall2 = flightSmall2[which(flightSmall2$model %in% modelNames),]
flightSmall2 = flightSmall2[which(flightSmall2$dest %in% destNames),]

trainSample2 = sample(nrow(flightSmall2), nrow(flightSmall2)*0.8)
flightTrain2 = flightSmall2[trainSample2,]
flightTest2 = flightSmall2[-trainSample2,]

# This test is using dep_delay

x2 = model.matrix(dep_delay~.,flightTrain2)[,-1]
xTest2 = model.matrix(dep_delay~.,flightTest2)[,-1]
y2 = flightTrain2$dep_delay
yTest2 = flightTest2$dep_delay

# LASSO
lasso.cv2 = cv.glmnet(x2,y2,alpha=1)
plot(lasso.cv2)  
lambda.cv2 = lasso.cv2$lambda.min
lambda.cv2 

fit.lasso2 = glmnet(x2,y2,alpha=1,lambda=lambda.cv2)

CF2 = as.matrix(coef(fit.lasso2, fit.lasso2$lambda.cv2))
CF2[CF2!=0,][order(CF2[CF2!=0])]
# Important vars: model, dest, manufacturer, carrier, month


pred.lasso2 = predict(fit.lasso2,newx=xTest2)
lasso_mse2 = mean((yTest2-pred.lasso2)^2)
cat("LASSO MSE is:", lasso_mse2)
cat("Number of predictors with LASSO:", sum(fit.lasso2$beta[,1] != 0), "out of",
    length(fit.lasso2$beta))
