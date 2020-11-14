library(tidyverse)
library(data.table)

custdata <- readRDS("Custdata/custdata.RDS") %>% setDT()
(custdata[, .N, by="age"] %>% setnames(c("age", "cnt")))[order(-rank(cnt), age) ]
custAgeCount <- custdata[, .N, by="age"] %>% setnames(c("age", "cnt"))

(custdata[income < 0, lapply(.SD, mean), by="age", .SDcols="income"] %>% 
      setnames(c("age", "income")))[order(rank(income))] -> negIncomes
is(negIncomes)
# Melt the negIncome dt...
melt(negIncomes)

DT <- copy(USArrests) %>% as.data.table(keep.rownames = TRUE) %>% setnames(c("rn"), c("State"))
# Consolidates three value-columns (Murder, Rape, Assauls, Urban Population) into a
# single variable column. This is necessary to transform this data.table to 
# wider format by state. In order to make data more meaningful, consider expressing infraction 
# frequency vs urban population value. 
DTmelt <- melt(DT) %>% setnames(c("variable", "value"), c("Category", "value"))
DTwide <- dcast(DTmelt, Category ~ State, value.var = "value")





negIncomes2 <- custdata[income < 0, ]
negIncomes2 %>% ggplot() + geom_histogram(aes(x=age), bin=1) # doesnt look like negative income shows any meaningful pattern...

#Correcting "invalid" age ( age of 0 ) and income ( negative income amount ) in the table. 
custdata <- custdata %>% mutate(age = na_if(age, 0), income = ifelse(income < 0, NA, income))

# Correcting gas_usage, according to provided field description...
customer_data <- custdata %>% mutate(gas_with_rent = (gas_usage == 1), gas_with_electricity = (gas_usage == 2), no_gas_bill = (gas_usage == 3)) %>% mutate(gas_usage = ifelse(gas_usage < 4, NA, gas_usage))

# Results are identical...
count_missings(customer_data)
summary(customer_data)

nacounts <- count_missings(customer_data)
hasNA <- which(nacounts > 0) 

custdata[, unique(gas_usage)]

#Correcting Missing Housing_type values...
ht <- customer_data[, unique(housing_type)] 

# Using refactor pkg function
#install.packages("radiant.data")
#customer_data %>% refactor(customer_data$housing_type, ht, "_invalid_")

# Or obtain the level of the factor field first, and add an extra level for NA's 
# levels <- levels(customer_data[, housing_type])
# levels[length(levels) + 1] <- "_invalid_housing_type_"
# customer_data[, housing_type := lapply(.sd, factor, levels), .sdcols="housing_type"]
# customer_data[is.na(housing_type), housing_type:= "_invalid_housing_type_"]
 

# developing a simple treatment plan to address NAs in customer_data...
install.packages("vtreat")
library(vtreat)

varlist <- setdiff(colnames(customer_data), c("custid", "health_ins"))
t_plan1 <- design_missingness_treatment(customer_data, varlist = varlist)
training_prepared <- prepare(t_plan1, customer_data)

colnames(customer_data)
colnames(training_prepared)
is(training_prepared)

sapply(training_prepared, FUN=function(col) sum(is.na(col)))
training_prepared[, sum(is.na(.SD))]
customer_data[, lapply(.SD, function(x) sum(is.na(x)))][, sum(.SD)]


# Reviewing the result of applying treatment plan on customer_data
htmissing <- which(is.na(customer_data[, housing_type]))
customer_data[htmissing]
columns_to_look_at <- c("custid", "is_employed", "num_vehicles", "housing_type", "health_ins")
customer_data[htmissing, .SD, .SDcols = columns_to_look_at]
training_prepared[htmissing, .SD, .SDcols=columns_to_look_at]

customer_data[!is.na(num_vehicles), mean(num_vehicles)]

# Including each state`s median income level to the list. The income field will be normalized using the state income_level. 
median_income_table <- readRDS("Custdata/median_income.RDS")
training_prepared <- training_prepared %>% left_join(., median_income_table, by="state_of_res") 
training_prepared[, `income_Normalized`:=(`income`/`median_income`)]#[, income_Normalized :=V1]-> training_prepared

tr_pr2 <- customer_data[!is.na(income), lapply(.SD, median), by="state_of_res", .SDcols="income"] #%>% left_join(., median_income_table, by="state_of_res")
identical(tr_pr2[, median_income], tr_pr2[, income])
summary(custdata[state_of_res=="Illinois", income])

customer_data[state_of_res=="Illinois", income] -> v2


# Analyze age field, see if it needs to be normalized...
summary(training_prepared$age)
training_prepared[is.na(age), ] #None.
training_prepared[, normalized_Age := lapply(.SD, function(col) col/mean(col)), .SDcols="age"]

## NORMALIZATION TRANSFORMATION ##
# How many are in "much less than 1" and "much more than 1" groups?
training_prepared[, .(normalized_Age, normalized_Age < 1.0) ][, .(.N), by="V2"][, .("less than 1" = V2, "count"=N, "percentage"=N/sum(N))]
# Chart #1
training_prepared[, .N, by="normalized_Age"] %>% ggplot() + geom_bar(aes(x=normalized_Age, y=N), stat="identity")
# Chart #2  -- Cut normalizedAge into bins of 0.5 unit width...
training_prepared[, .(.SD, cutBin=cut(training_prepared$normalized_Age, seq(0,2.5, 0.2)))] %>% ggplot() + geom_histogram(aes(x=cutBin), stat="count")


## CENTERING & SCALING TRANSFORMATION 
##  - measuring the spread of measure (age, in this case) from its mean.
##  
training_prepared[, scaled_age := (age - mean(age))/sd(age) ]
#   - Trying out scale() 
dataf <- training_prepared[, c("age", "income", "num_vehicles", "gas_usage")]
summary(dataf)
dataf_scaled <- scale(dataf, center = TRUE, scale = TRUE)
summary(dataf_scaled)
attributes(dataf_scaled)
attr(dataf_scaled, 'scaled:center')
attr(dataf_scaled, 'scaled:scale')
summary(dataf_scaled[, "age"])
summary(training_prepared$scaled_age)
is(dataf_scaled)

# Keep attributes to "scaled" matrix, to be reused when "scaling" another batch of data. 
(sds <- attr(dataf_scaled, "scaled:scale"))
(means <- attr(dataf_scaled, "scaled:center"))
##  eg  new_scaled_variables <- scale(custdata[, c("age", "income", "num_vehicles", "gas_usage")], center=means, scale=sds)


# Scaling by logarithms 
##    - Oftentimes, the base of logs does not matter, the scale of values may differ,
##    but the patterns depicted by the values will exhibit similar characteristics.
##    Monetary values are often transformed using 10-based logs. 
##    - If a measure's value ranges by several orders of magnitude, log-transformation is
##    a good idea, especially in a ML application where excessively wide range of values will 
##    not be favored.
##    -- *** Rememeber to preprrocess values to eliminate negative values. Log Transform is 
##    only applicable to non-negative values. { NaN, x | x < 0 }, and 0->Inf.
## Two types of processes to produce data measures are ,  
##    1. multiplicative process, and ( GDP, interest earning. 5% salary increase across the board. 
##      The $amt will be different for everyone. )
##    2. additive process. ( Weight Loss based on calorie intake, account balance, $5.00-Off coupon.
##      )

install.packages("scales")
library(scales)

ggplot(training_prepared, aes(x=income)) + geom_density() + scale_x_continuous(labels=dollar)
  # geom_density is basically a smooth line version of geom_histogram.
ggplot(training_prepared, aes(x=income)) + geom_density() + 
  scale_x_log10(breaks=c(10, 100, 1000, 10000, 100000, 1000000), labels=dollar) + 
  annotation_logticks(sides="b", color="gray")
  # so the above chart and geom_histogram should exhibit similar chart. And it does.
ggplot(training_prepared, aes(x=income)) + geom_histogram() + 
  scale_x_log10(breaks=c(10, 100, 1000, 10000, 100000, 1000000), labels=dollar) + 
  annotation_logticks(sides="b", color="gray")

custdata_test <- setDT(readRDS("Custdata/custdata.RDS"))
custdata_test[, .(income, signedLog10(income))][V2==0, unique(income)]



## Split dataset into a training and test sample sets.
#     training 90%
#     test     10%
#   1.  Reproducible sampling is critical in development, testing. Set seed before sampling. 
#   2.  Also another item is to store grouping column, 'gp' in this case, with the data set. 
set.seed(25643) 
customer_data[, `gp` := runif(.N)]
customer_data[, unique(gp)]
customer_test <- customer_data[gp <= 0.1, ]
customer_training <- customer_data[gp > 0.1, ]
dim(customer_test)

## Sampling by household.
household_data <- setDT(readRDS("Custdata/hhdata.RDS"))
set.seed(243674)
hh <- household_data[, .(`household_id` = unique(household_id))][, gp:=runif(.N)]
household_dataset <- household_data %>% left_join(hh, by="household_id")
dim(customer_training)
household_test <- household_dataset[gp <=0.1, ]
household_training <- household_dataset[gp > 0.1, ]
dim(household_test)
dim(household_training)

install.package(writexl)
library(writexl)
write_xlsx(training_prepared, path="exportDataSets/CustomerData/training_prepared.xlsx")





















