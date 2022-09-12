library(lubridate)
library(data.table)

## Function to retrieve data from API
get_bioportal <- function(url){
  setDT(jsonlite::fromJSON(
    rawToChar(
      httr::GET(url, httr::content_type('application/json'),
                httr::add_headers('Accept-Enconding'="br"))$content)
  ))
}

first_day <- make_datetime(2020, 3, 12, 0, 0, 0, tz= "America/Puerto_Rico")

last_day <- now(tz= "America/Puerto_Rico") + days(1)

the_years <- seq(2020, year(today()))

## Load latest data
message("Downloading dataset.")

## filter by date example: ?createdAtStartDate=2021-09-09T04:00:00Z&createdAtEndDate=2021-09-10T04:00:00Z
cases_url <- "https://bioportal.salud.pr.gov/api/administration/reports/orders/basic"

first_day<- first_day |>
  with_tz(tzone = "GMT") |>
  format("%Y-%m-%dT%H:%M:%SZ")


last_day<- last_day |>
  with_tz(tzone = "GMT") |>
  format("%Y-%m-%dT%H:%M:%SZ")


## generate the queries 
cases_url_molecular <-  paste0(cases_url,
                               "?testType=Molecular",
                               "&createdAtStartDate=",
                               first_day,
                               "&createdAtEndDate=",
                               last_day)

cases_url_antigens <- paste0(cases_url,
                             "?testType=Antigens",
                             "&createdAtStartDate=",
                             first_day,
                             "&createdAtEndDate=",
                             last_day)


# Reading and wrangling cases data from database ---------------------------
message("Reading case data.")

tests_molecular <- get_bioportal(cases_url_molecular)
tests_antigens <- get_bioportal(cases_url_antigens)
tests <- rbind(tests_molecular, tests_antigens)
tests <- data.frame(tests)
save(tests, file = "tests.RData")
#once we are done with wrangling, we will erase the two tables, for now keep in case we need to rebuild tests
#rm(tests_molecular, tests_antigens); gc(); gc()


## use ymd_hms function to convert dates to dates. Make sure to make the timezone America/Puerto_Rico
## Tasks: take a look at table(all_tests_with_id$result) and figure out which are positive, which negative,
## and which inconclusive. Once you do, use the function fcase to redefine these values so that they are 
## either positive, negative or other
## the tolower and grepl functions will be useful.

# Coverting dates to Puerto_Rico Time Zone
date_var <- c("collectedDate", "reportedDate", "orderCreatedAt", "resultCreatedAt")
for(dv in date_var){
  tests[, dv] <- ymd_hms(tests[, dv], tz= "America/Puerto_Rico")
}

# Changing labels to positive, negative, or other
org_names <- unique(c(tests_antigens$result, tests_molecular$result))


tests[, "result"] <- fcase(
  
  # I didn't use grep here because of the small number of possibilities and 
  # the option to only be posistive for influenza
  
  tests[, "result"] %in% c("Negative", "Presumptive Negative",
                           "COVID-19 Negative", "SARS-CoV-2 Negative"), "negative",
  
  tests[, "result"] %in% c("Positive", "Presumptive Positive", "Positive 2019-nCoV", 
                           "COVID-19 Positive", "SARS-CoV-2 Positive", "SARS-CoV-2 Presumptive Positive", 
                           "Positive for COVID-19", "Positive for influenza A and COVID-19"), "positive",
  
  tests[, "result"] %in% c("Other", "Not Valid", "Not Detected", "Inconclusive", "Not Tested", "Invalid",
                           "Positive for influenza A", "Not tested", "Positive IgG Only"), "other"
)

table(tests[, "result"])


## covert the age range to a factor
# Would it be desired to order these factors for the future?
tests[, "ageRange"] <- ifelse(tests[, "ageRange"] == "N/A", NA, tests[, "ageRange"])
tests[, "ageRange"] <- as.factor(tests[, "ageRange"])

## save the object at the end into a directory called rdas. Do no upload these data to GitHub as it will be too big



