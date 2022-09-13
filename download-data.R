library(lubridate)
library(data.table)

## Function to retrieve data from API
get_bioportal <- function(url){
  message("Downloading query ", url)
  setDT(jsonlite::fromJSON(
    rawToChar(
      httr::GET(url, httr::content_type('application/json'),
                httr::add_headers('Accept-Enconding'="br"))$content)
  ))
}

first_day <- make_datetime(2020, 3, 12, 0, 0, 0, tz= "America/Puerto_Rico")
last_day <- now(tz= "America/Puerto_Rico") + days(1)

the_years <- seq(2020, year(today()))

the_days <- unique(c(first_day, make_date(the_years[-1], 1, 1), last_day))
the_days <- the_days |> with_tz(tzone = "GMT") |> format("%Y-%m-%dT%H:%M:%SZ")

## Load latest data
message("Downloading dataset.")

## filter by date example: ?createdAtStartDate=2021-09-09T04:00:00Z&createdAtEndDate=2021-09-10T04:00:00Z
cases_url <- "https://bioportal.salud.pr.gov/api/administration/reports/orders/basic"

## Define a temporaty matrix saving the test type, start and end date for query
tmp <- merge(data.frame(test_type = c("Molecular", "Antigens")), 
             data.frame(start = head(the_days, -1), end =  tail(the_days,-1)))

## Define the queries
queries <- apply(tmp, 1, function(x)
  paste0(cases_url,
         "?testType=", x[1],
         "&createdAtStartDate=", x[2], "&createdAtEndDate=", x[3]))


# Reading and wrangling cases data from database ---------------------------
message("Reading case data.")

tests <- lapply(queries, get_bioportal)
tests <- do.call(rbind, tests) |> unique() ##unique removes duplicate rows

## save(tests, file = "tests.RData")
## RI: I recommend creating a directory, rdas, to store all R objects. there might be more in the future and you
## want them organized in one dir, out of the way from you scripts.
## Also, to avoid a possible unwanted overwrite when loading, I prefer saveRDS

saveRDS(tests, file = "rdas/raw-tests.rds")
