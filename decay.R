options(scipen=999)
open <- 110000000
drop <- c(.462,.49,.274,.306,.429, .257,.426,.391)
weekendNum <- 26
decay <- .35
percentWeekDay <- 1.7

cumulative <- open
total <- data.frame(weekend = numeric(weekendNum), cumulativeTotal = numeric(weekendNum))
total[1,1] <- cumulative
total[1,2] <- cumulative
a <- 1
weeksToCalc <- weekendNum-length(drop)
  for (p in drop){
    a <- a+1
nextweek <- total[(a-1),1]-total[(a-1),1]*p
total[a,1] <- nextweek
cumulative <- nextweek+cumulative
total[a,2] <- cumulative
  }
for(c in c(1:weeksToCalc)){
  a <- a+1
  nextweek <- total[(a-1),1]-total[a-1,1]*decay
  total[a,1] <- nextweek
  cumulative <- nextweek+cumulative
  total[a,2] <- cumulative
}

total <- round(total, digits = 0)

total$includeWeekday <- c(total$cumulativeTotal[1],(total$cumulativeTotal[c(2:nrow(total))]*percentWeekDay))

#multiplier
total[nrow(total),2] / open

plot(total$weekend)
