library(lubridate)
library(data.table)

tests <- readRDS("rdas/raw-tests.rds")
## Remove data entry errors with test data before the test was order
tests <- tests[collectedDate <= orderCreatedAt]

## Converting ID to factor
tests[, patientId := factor(patientId)]

## Convert times to GMT to Puerto Rico time
cols <- c("collectedDate", "reportedDate", "orderCreatedAt", "resultCreatedAt")
tests[, (cols) := lapply(.SD, ymd_hms, tz= "America/Puerto_Rico"), .SDcols = cols]


## Convert test result to TRUE (positive test), FALSE (negative test) 
## Remove the rows with inconclusive tests (labeled other)
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
tests <- tests[result != "other"]
tests[, result := result == "positive"]                          

## Change ageRange to groups of 10 years and 80 and above
cuts <- c(seq(0, 80, 10), Inf)
labels <- stringr::str_replace(paste(head(cuts, -1), tail(cuts, -1)-1, sep = "-"), "-Inf", "+")
tests[, ageRange := 
        cut(as.numeric(stringr::str_extract(tests$ageRange, "^\\d+")), breaks = cuts, labels = labels, right=FALSE)]

## Make region a factor
tests[, region := factor(dplyr::na_if(region, "N/A"))]

## Order by day of test within each person
tests <- tests[order(patientId, collectedDate)]

## Define day as days since start of data base
## Keep only tests is not missing and is not before pandemic started (likely data entry error)

first_day <- make_date(2020, 3, 12)
tests[, day := as.numeric(as_date(collectedDate)) - as.numeric(first_day) + 1]

tests <- tests[!is.na(day)]
tests <- tests[day >= 1]
## Number of positive tests and number of negative tests
tests <- tests[, npos := sum(result), keyby = patientId]
tests <- tests[, nneg := sum(!result), keyby = patientId]

## RI: define a function that computes minium distance between any two positives
## RI: then clusters infections  into groups
cluster_infections <- function(x, gap = 60){
  dist <- outer(x, x, FUN = "-") ##distance between each test
  dist[upper.tri(dist, diag = TRUE)] <- NA #we only care about comparison to future tests not past tests
  dist <- matrixStats::rowMins(dist, na.rm=TRUE) #take smallest distance for each test
  return(cumsum(dist >= gap)) #if new infections use new number
}
## cluster positive tests into infections if they are within 60 days of each other
## The code easily permits changing 60 to some other length of time
tests[npos > 1 & result, infection := cluster_infections(day), by = patientId]
tests[npos == 1 & result, infection := 1]
## fill in infection for negative tests
tests[npos >= 1, infection := zoo::na.locf(infection, na.rm = FALSE), by = patientId]


## Find the number of days since last negative test before each positive test
dist_to_prev_neg <- function(day, result){
  dist <- outer(day[result], day[!result], FUN = "-")
  dist[dist<=0] <- NA
  res <- rep(NA, length(day))
  res[result] <- matrixStats::rowMins(dist,na.rm=TRUE)
  res[is.infinite(res)]<-NA
  return(res)
}
tests[npos>0 & nneg >0, dist_prev_neg := dist_to_prev_neg(day, result), by = patientId]

## for debugging
# for(i in unique(tests[npos>1]$patientId)){
#   tests[patientId==i, c("npos", "nneg", "testType", "result", "day", "infection", "dist_prev_neg")] |> View()
#   scan()
# }

saveRDS(tests, file = "rdas/tests.rds")




