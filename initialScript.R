library(nycflights13)
library(tidyverse)
library(plyr)



data(flights)
data(weather)
data(airports)
data(airlines)
data(planes)
head(flights)
head(weather)
head(airports)
head(airlines)
head(planes)

sum(is.na(flights[4]))
sum(is.na(flights[6]))
sum(is.na(flights[7]))
sum(is.na(flights[9]))
sum(is.na(flights[12]))
sum(is.na(flights[15]))

# Flights

# This gets rid of all NAs; presumably all canceled flights. Still have 327,000
# flights so not a big deal to get rid of them
view(flights[which(is.na(flights[c(4,6,7,9,12,15)])),])
flights2 = flights[which(!is.na(flights[15])),]
# Although...might take out canceled flights...do we consider those delayed?



boxplot(flights2$dep_delay[which(flights2$dep_delay > 20)]~flights2$month[
  which(flights2$dep_delay > 20)])
mean(flights2$dep_delay[which(flights2$dep_delay > 20)])

# kind of more in summer? More Vacations? 

boxplot(flights2$dep_delay[which(flights2$dep_delay > 20)]~
          flights2$origin[which(flights2$dep_delay > 20)], outline=F)

boxplot(flights2$dep_delay[which(flights2$dep_delay > 200 & 
                             flights2$dep_delay > 0)]~
          flights2$origin[which(flights2$dep_delay > 200 & 
                                  flights2$dep_delay > 0)])
# Pretty much the same from different airports. Like, exactly same when not 
# considering outliers


# boxplot(flights2$dep_delay[which(flights2$dep_delay > 20)]~
#           flights2$dest[which(flights2$dep_delay > 20)], outline=F)
# 
# boxplot(flights2$dep_delay[which(flights2$dep_delay > 200 & 
#                                    flights2$dep_delay > 0)]~
#           flights2$dest[which(flights2$dep_delay > 200 & 
#                                   flights2$dep_delay > 0)])
# Too many destinations to look at rn

plot(flights2$dep_delay[which(flights2$dep_delay > 20)]~
          flights2$air_time[which(flights2$dep_delay > 20)])

plot(flights2$dep_delay[which(flights2$dep_delay < 200 & 
                                   flights2$dep_delay > 0)]~
          flights2$air_time[which(flights2$dep_delay < 200 & 
                                flights2$dep_delay > 0)])
# Airtime basically worthless except to tell us that there are no airports 
# 8-9 hours away from NY (all the 10+ hr are to Hawaii)

plot(flights2$dep_delay[which(flights2$dep_delay > 20)]~
       flights2$distance[which(flights2$dep_delay > 20)])

plot(flights2$dep_delay[which(flights2$dep_delay < 300 & 
                                flights2$dep_delay > 0)]~
       flights2$air_time[which(flights2$dep_delay < 300 & 
                                 flights2$dep_delay > 0)])
# Same deal here for the most part (airtime is gonna be like 1:1 with distance
# so duh)

boxplot(flights2$dep_delay[which(flights2$dep_delay > 20)]~
       flights2$hour[which(flights2$dep_delay > 20)])

boxplot(flights2$dep_delay[which(flights2$dep_delay < 200 & 
                                flights2$dep_delay > 20)]~
       flights2$hour[which(flights2$dep_delay < 200 & 
                                 flights2$dep_delay > 20)])
# Seems like delays start going down around 10 and 11, as well as 5. Note 0-4
# not scheduled hours. Very small difference though, and changes depending on 
# what parameters I look at it with






boxplot(flights2$dep_delay~flights2$carrier)

boxplot(flights2$dep_delay[which(flights2$dep_delay > 20)]~flights2$carrier[
  which(flights2$dep_delay > 20)])

boxplot(flights2$dep_delay[which(flights2$dep_delay > 20)]~flights2$carrier[
  which(flights2$dep_delay > 20)], outline=F)

boxplot(flights2$dep_delay[which(flights2$dep_delay < 200 & flights2$dep_delay > 0)]~
          flights2$carrier[which(flights2$dep_delay < 200 & flights2$dep_delay > 0)])
# HA and OO killing it -- not even any outliers. AS and F9 also particularly 
# good. However, If we take out super long 



# ----------------------------------
flightsPlanes = left_join(flights2, planes, by="tailnum")

boxplot(flightsPlanes$dep_delay[which(flightsPlanes$dep_delay > 20)]~
          flightsPlanes$manufacturer[which(flightsPlanes$dep_delay > 20)])

boxplot(flightsPlanes$dep_delay[which(flightsPlanes$dep_delay < 200 & 
                                        flightsPlanes$dep_delay > 20)]~
          flightsPlanes$manufacturer[which(flightsPlanes$dep_delay < 200 & 
                                             flightsPlanes$dep_delay > 20)])
# Too tired to decide if this means anything. Thought there would be fewer 
# manufacturers

boxplot(flightsPlanes$dep_delay[which(flightsPlanes$dep_delay > 20)]~
          round_any(flightsPlanes$seats[which(flightsPlanes$dep_delay > 20)],
                    50,f=round))
table(round_any(flightsPlanes$seats[which(flightsPlanes$dep_delay > 20)],
                50,f=round))

boxplot(flightsPlanes$dep_delay[which(flightsPlanes$dep_delay < 200 & 
                                        flightsPlanes$dep_delay > 20)]~
          round_any(flightsPlanes$seats[which(flightsPlanes$dep_delay < 200 & 
                                             flightsPlanes$dep_delay > 20)],
                    50,f=round))
table(flightsPlanes$engine)
# Way more 2 engine failures but I'm pretty sure there 



corr_simple <- function(data=df,sig=0.5){
  
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  df_cor <- df_cor %>% mutate_if(is.integer, as.numeric)
  corr <- cor(df_cor)
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  corr[corr == 1] <- NA 
  corr <- as.data.frame(as.table(corr))
  corr <- na.omit(corr) 
  corr <- subset(corr, abs(Freq) > sig) 
  corr <- corr[order(-abs(corr$Freq)),] 
  print(corr)
}

corr_simple(flights2[-19])


flights2$sigDelay = ifelse(flights2$dep_delay >= 20, 1, 0)

sum(flights2$sigDelay)


table(flights2$carrier)[order(table(flights2$carrier),decreasing=T)][1:9]
sum(table(flights2$carrier)[order(table(flights2$carrier),decreasing=T)][1:9])/nrow(flights2)
# Top 9 carriers make up 97% of flights, we will just look at those

carrierNames = names(table(flights2$carrier)[order(table
                                (flights2$carrier),decreasing=T)][1:9])

flights2 = flights2[which(flights2$carrier %in% carrierNames),]


#  Consider focusing on only top 9 carriers
