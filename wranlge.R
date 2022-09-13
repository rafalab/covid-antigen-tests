library(lubridate)
library(data.table)

tests <- readRDS("rdas/raw-tests.rds")
## use ymd_hms function to convert dates to dates. Make sure to make the timezone America/Puerto_Rico
## Tasks: take a look at table(all_tests_with_id$result) and figure out which are positive, which negative,
## and which inconclusive. Once you do, use the function fcase to redefine these values so that they are 
## either positive, negative or other
## the tolower and grepl functions will be useful.

# # Coverting dates to Puerto_Rico Time Zone
# date_var <- c("collectedDate", "reportedDate", "orderCreatedAt", "resultCreatedAt")
# for(dv in date_var){
#   tests[, dv] <- ymd_hms(tests[, dv], tz= "America/Puerto_Rico")
# }

## RI: In data.table you can use apply the same function to several columns using the .SD (subset of data) symbol
cols <- c("collectedDate", "reportedDate", "orderCreatedAt", "resultCreatedAt")
tests[, (cols) := lapply(.SD, ymd_hms, tz= "America/Puerto_Rico"), .SDcols = cols]



# # Changing labels to positive, negative, or other
# org_names <- unique(c(tests_antigens$result, tests_molecular$result))
# 
# 
# tests[, "result"] <- fcase(
#   
#   # I didn't use grep here because of the small number of possibilities and 
#   # the option to only be posistive for influenza
#   
#   tests[, "result"] %in% c("Negative", "Presumptive Negative",
#                            "COVID-19 Negative", "SARS-CoV-2 Negative"), "negative",
#   
#   tests[, "result"] %in% c("Positive", "Presumptive Positive", "Positive 2019-nCoV", 
#                            "COVID-19 Positive", "SARS-CoV-2 Positive", "SARS-CoV-2 Presumptive Positive", 
#                            "Positive for COVID-19", "Positive for influenza A and COVID-19"), "positive",
#   
#   tests[, "result"] %in% c("Other", "Not Valid", "Not Detected", "Inconclusive", "Not Tested", "Invalid",
#                            "Positive for influenza A", "Not tested", "Positive IgG Only"), "other"
# )
# 
# table(tests[, "result"])

## RI: Using data.table syntax you can write this a bit more succinctly
## I also realized that using fct_collapse you can make the operation much faster than
## using fcast, by changing the factor names rather
## then each entry separately. 
tests[, result := factor(tolower(result))] #using to lower in case a difference in case slips in
tests[, result := 
        forcats::fct_collapse(result,
                              negative = c("negative", "presumptive negative",
                                           "covid-19 negative", "sars-cov-2 negative"),
                              positive = c("positive", "presumptive positive", "positive 2019-ncov", 
                                           "covid-19 positive", "sars-cov-2 positive", 
                                           "sars-cov-2 presumptive positive", 
                                           "positive for covid-19", "positive for influenza a and covid-19"),
                              other_level = "other")]
                              


# ## covert the age range to a factor
# # Would it be desired to order these factors for the future?
# tests[, "ageRange"] <- ifelse(tests[, "ageRange"] == "N/A", NA, tests[, "ageRange"])
# tests[, "ageRange"] <- as.factor(tests[, "ageRange"])

## RI: The idea was to from a factor that will help with the analysis and ordered by age.
## Note that using factor will make 100 be younger than 20 due to alphabetical ordering.
cuts <- c(seq(0, 80, 10), Inf)
labels <- stringr::str_replace(paste(head(cuts, -1), tail(cuts, -1)-1, sep = "-"), "-Inf", "+")

tests[, ageRange := 
        cut(as.numeric(stringr::str_extract(tests$ageRange, "^\\d+")), cuts, labels = labels, right=FALSE)]

## RI: making a region a actor as well to save space
tests[, region := factor(dplyr::na_if(region, "N/A"))]

### Next tasks: 
## 1) make sure to review data.table intros. Here is one to start: https://atrebas.github.io/post/2019-03-03-datatable-dplyr/
## 2) Use data.table approach to count how many tests each individual received. add a column `n` to keep this info 
## 3) For each individual, create an index dividing the tests into `infections`. Define a new infection anytime you have more than 90 days between positive tests. Use data.table appraoch
## 4) For each individual, add a column that keeps number of days since last negative test.

