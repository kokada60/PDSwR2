library(tidyverse)
library(data.table)

custdata <- readRDS("Custdata/custdata.RDS") %>% setDT()
(custdata[, .N, by="age"] %>% setnames(c("age", "cnt")))[order(-rank(cnt), age) ]
custAgeCount <- custdata[, .N, by="age"] %>% setnames(c("age", "cnt"))

(custdata[income < 0, lapply(.SD, mean), by="age", .SDcols="income"] %>% setnames(c("age", "income")))[order(rank(income))] -> negIncomes

negIncomes <- custdata[income < 0, ]
negIncomes %>% ggplot() + geom_histogram(aes(x=age), bin=1) # doesnt look like negative income shows any meaningful pattern...

#Correcting "invalid" age ( age of 0 ) and income ( negative income amount ) in the table. 
custdata <- custdata %>% mutate(age = na_if(age, 0), income = ifelse(income <= 0, NA, income))

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

varlist <- setdiff(colnames(customer_data), c("cus", "health_ins"))
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
training_prepared <- training_prepared[, income_normalized := income/median_income]

tr_pr2 <- customer_data[!is.na(income), lapply(.SD, median), by="state_of_res", .SDcols="income"] #%>% left_join(., median_income_table, by="state_of_res")
identical(tr_pr2[, median_income], tr_pr2[, income])
summary(custdata[state_of_res=="Illinois", income])

customer_data[state_of_res=="Illinois", income] -> v2


































