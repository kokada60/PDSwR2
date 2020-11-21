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


airqualityDT_with_date <- (airquality %>% setDT())[, date := dmy(datestr(Day, Month, 1973))]
ggplot(airqualityDT_with_date, aes(x=date, y=Ozone)) + geom_point() + geom_line() + xlab("Date") + 
  ggtitle("Daily Ozone Readings")
# Apparently there are NA's in ozone column. 
library(zoo)
airquality_corrected <- airqualityDT_with_date[, OzoneCorrected:=na.locf(Ozone, na.rm = FALSE)]
naOzone <- airquality_corrected[is.na(Ozone), which=TRUE]
airquality_corrected_naOzone <- 
  airquality_corrected[naOzone, 
                       .(naOzoneID=naOzone, prevOzone=airquality_corrected[naOzone-1, Ozone], 
                         Ozone, OzoneCorrected, date)]
# airquality_corrected[is.na(Ozone), 
#                      .(Ozone, prevOzone=shift(Ozone, 1L, "lag"), OzoneCorrected, date)]
# airquality_corrected[airquality_corrected[is.na(Ozone), which=TRUE], 
#                      .(Ozone, prevOzone=airquality_corrected[1, Ozone], OzoneCorrected, date)]

splitAQByMonth <- split(airquality_corrected_naOzone, m(airquality_corrected_naOzone$date))


## Rolling Joins...
quotes <- data.table(
  bid = c(5, 5, 7, 8),
  ask = c(6, 6, 8, 10),
  bid_quantity = c(100, 100, 100, 100),
  ask_quantity = c(100, 100, 100, 100),
  when = as.POSIXct(strptime(
    c("2018-10-18 1:03:17", 
      "2018-10-18 2:12:23", 
      "2018-10-18 2:15:00", 
      "2018-10-18 2:17:51"), 
    "%Y-%m-%d %H:%M:%S")))



trades <- data.table(
  trade_id = c(32525, 32526),
  price = c(5.5, 9),
  quantity = c(100, 200),
  when = as.POSIXct(strptime(
    c("2018-10-18 2:13:42", 
      "2018-10-18 2:19:20"), 
    "%Y-%m-%d %H:%M:%S")))

quotes[, quote_time := when ]
trades[, trade_time := when ]
quotes[ trades, on = "when", roll = TRUE][, .(quote_time, bid, price, ask, trade_id, trade_time)]


#Data Transformation...
install.packages("xts")
library(datasets)
library(xts)
Seatbelts


SeatbeltsDT <- data.table(Seatbelts)
dates <- index(as.xts(time(Seatbelts)))
SeatbeltsDT[, date:=dates][, law := ifelse(law==1, "new law", "pre-law") ]
SeatbeltsDTFiltered <- SeatbeltsDT[(date >= as.yearmon("Jan 1982") & date <= as.yearmon("Dec 1983") ), drop = FALSE][, date := as.Date(date)]
SeatbeltsDTFiltered %>% ggplot(aes(x = date, y= DriversKilled, color = law, shape = law)) + 
    geom_point() + geom_smooth(se=FALSE) + ggtitle("UK car driver")

  ggplot(aes(x=date, y=DriversKilled, color=law, shape=law)) + geom_smooth(se=FALSE) + geom_point() +
  ggtitle("UK Drivers monthly accidents count")
Seat

install.packages("facetscales")
library(facetscales)

SeatbeltsDTMelted <- SeatbeltsDTFiltered %>% 
                      melt(id.vars = NULL, measure.vars = c("DriversKilled", "front", "rear"), 
                      variable.name = "victim_type", value.name = "nvictims")

SeatbeltsDTMelted2 <- SeatbeltsDTFiltered %>% 
  melt(id.vars = c("drivers", "kms", "PetrolPrice", "VanKilled", "law", "date"), measure.vars = c("DriversKilled", "front", "rear"), 
       variable.name = "victim_type", value.name = "nvictims")

SeatbeltsDTMelted %>% ggplot(aes(x=date, y=nvictims, color=law, shape=law)) + 
                    geom_smooth(se=FALSE) + #scale_y_continuous(limits=c(0, 250)) + 
                    geom_point() + facet_wrap( ~victim_type, ncol=1, scales = "free_y") + 
                    ggtitle("UK Auto Fatality by victim's seating position and month")



# dcast 
ChickWeightDT <- data.table(ChickWeight)
summary(ChickWeightDT)
levels(ChickWeight$Chick)
unique(ChickWeight$Chick)
formatIDDigits <- function(x, strPrefix, n) {
  formatC(as.integer(x), width=n, flag="0") %>% paste0(strPrefix, .)
}
ChickWeightDT[, ChickID:=formatIDDigits(as.character(Chick), "Chick", max(nchar(as.character(Chick))))]
ChickSummary <- ChickWeightDT[, .(count=.N, weight=mean(weight), 
                 q1_weight=quantile(weight, probs = 0.25), 
                 q2_weight=quantile(weight, probs = 0.75),
                 max_weight=max(weight)), by="Time"]
ChickSummary_Long <- ChickSummary %>% 
          melt(id.vars = c("Time", "q1_weight", "q2_weight", "max_weight"), 
               measure.vars = c("count", "weight"), 
               variable.name="measurement", value.name="value")
ChickSummary1 <- cdata::unpivot_to_blocks(ChickSummary, nameForNewKeyColumn="measurement", 
                                          nameForNewValueColumn="value", 
                                          columnsToTakeFrom = c("count", "weight"))
ChickSummary_Long[measurement=="count", q1_weight:=NA][measurement=="count", q2_weight:=NA]
ChickSummary_Long %>% ggplot(aes(x=Time, y=value, color=measurement)) + 
    geom_line(size=2) +  
    geom_line(data=ChickWeightDT[, .(Time, weight, measurement="weight", ChickID)], aes(x=Time, y=weight, group = ChickID), color="LightGray") + 
    geom_ribbon(aes(ymin=q1_weight, ymax=q2_weight), alpha=0.4, colour=NA) + 
    facet_wrap(~measurement, ncol = 1, scales="free")

ChickWeightDT_Wide <- ChickWeightDT %>% 
    data.table::dcast(formula = Chick ~ Time, value.var="weight")


controlTable <- wrapr::qchar_frame(
  "flower_part", "Length"     , "Width"     |
    "Petal"    , Petal.Length , Petal.Width |
    "Sepal"    , Sepal.Length , Sepal.Width )

transform <- rowrecs_to_blocks_spec(
  controlTable,
  recordKeys = c("iris_id", "Species"))

# do the unpivot to convert the row records to block records
iris <- data.frame(iris)
iris$iris_id <- seq_len(nrow(iris))
iris_aug <- iris %.>% transform

ggplot(iris_aug, aes(x=Length, y=Width)) +
  geom_point(aes(color=Species, shape=Species)) + 
  facet_wrap(~flower_part, labeller = label_both, scale = "free") +
  ggtitle("Iris dimensions") +  scale_color_brewer(palette = "Dark2")


                 