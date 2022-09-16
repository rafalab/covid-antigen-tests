#with(tests[!is.na(infection)][1:100,], paste(patientId, infection))

tmp <- tests[!is.na(infection) & npos >= 2]

## new id splitting individuals by infection
tmp[,patientId := factor(paste(as.numeric(patientId), infection, sep = "_"))]
tmp[,date:=as_date(collectedDate)]

compute_agreement <- function(date, result, day, testType){
  data.frame(date = date[-1],
             agreement = result[-1] == result[1], 
             lag = day[-1] - day[1], 
             first_test = rep(testType[1], length(testType)-1),
             test_type = testType[-1])
}


res <- tmp[, compute_agreement(date, result, day, testType), keyby =patientId]
res <- res[order(first_test, test_type, date, lag)]    

tmp2 <- res[, .(k = sum(agreement), n = .N), keyby = .(first_test, test_type, date, lag)]
tmp2 <- merge(CJ(date=seq(min(tmp2$date), max(tmp2$date), by = "day"),
                first_test = unique(tmp2$first_test),
                test_type = unique(tmp2$test_type),
                lag = 0:30),
              tmp2, by = c("first_test", "test_type", "date", "lag"), all.x = TRUE)
tmp2[is.na(tmp2)] <- 0
                
                
tmp2[,`:=`(k = zoo::rollsum(k, 30, na.pad = TRUE), n =  zoo::rollsum(n, 30, na.pad = TRUE)), 
           keyby = .(first_test, test_type, lag)]
           
tmp2[, prop := k/n]

library(ggplot2)
tmp2[lag == 1] |> 
  ggplot(aes(date, prop, color = test_type)) +
  geom_line() +
  geom_point() +
  facet_wrap(~first_test)

##fix
tmp2[lag <= 30, .(prop = mean(agreement)), keyby = .(first_test, test_type, lag)] |>
  ggplot(aes(lag, prop, color = test_type)) +
  geom_line() +
  geom_point() +
  facet_wrap(~first_test)

