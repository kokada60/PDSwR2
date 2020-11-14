library(tidyverse)
library(data.table)

ggplot(iris, aes(x=Petal.Length, y=Petal.Width)) + 
  geom_jitter(aes(color=Species, shape=Species), size=1.5) + 
  ggtitle("Petal Dimension of Iris Species : all measurements")

irisT <- as.data.table(iris, keep.rownames = TRUE) # If the data.frame does not have named rows, then the option isnt necessary.
# enclosing j in a list prevents a single column query from returning the result as a vector.
irisT[, .(`Petal.Length`, `Petal.Width`)]
c_desired <- c("Petal.Length", "Petal.Width", "Species")
irisT[, ..c_desired]s
irisT[, c(1,2)]

DT <- copy(USArrests) %>% as.data.table(keep.rownames = TRUE) %>% setnames("rn", "State")
DTmelted <- melt(DT)
DTwide <- dcast(DTmelted, variable ~ State, value.var = "value") %>% setnames("variable", "Category")
DTwide <- DTwide[, Category := lapply(.SD, factor, c("Assault", "Murder", "Rape", "UrbanPop")), .SDcols = "Category"][order(Category)]


## Strategy to deal with missing values. Use msleep dataset.
mDT <- as.data.table(msleep)
mDT[complete.cases(mDT), ]
# omit any row with NA in any column...

#testing out cumSum function. Build scrambled list of days and hours

numberOfHoursInEachDay <- c(2, 1, 4, 3, 3, 1)  # Day1-2 hour slots, Day2-1 hour slot, Day3-4 hour slots...
bDays <- rep(paste("day", seq(1:6)), numberOfHoursInEachDay)
bHours <- unlist(lapply(numberOfHoursInEachDay, function(x) seq(1:x)))
bHourlyPurchase <- sample(10:900, sum(numberOfHoursInEachDay), replace = FALSE)
dayhoursDT <- data.table(dy=bDays, hr=bHours, purc=bHourlyPurchase)
dayhoursDT_Scrambled <- dayhoursDT[sample(c(1:.N)), ]
dayhoursDT[order(dy, hr)]
dayhoursDT[, .(purc, runningTotal=cumsum(purc)), by=dy]



