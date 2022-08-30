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
tests <- rbind(all_tests_with_id_molecular, all_tests_with_id_antigens)
#once we are done with wrangling, we will erase the two tables, for now keep in case we need to rebuild tests
#rm(tests_molecular, tests_antigens); gc(); gc()


## use ymd_hms function to convert dates to dates. Make sure to make the timezone America/Puerto_Rico
## Tasks: take a look at table(all_tests_with_id$result) and figure out which are positive, which negative,
## and which inconclusive. Once you do, use the function fcase to redefine these values so that they are either positive, negative or other
## the tolower and grepl functions will be useful.

## covert the age range to a factor

## save the object at the end into a directory called rdas. Do no upload these data to GitHub as it will be too big



