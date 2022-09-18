#with(tests[!is.na(infection)][1:100,], paste(patientId, infection))

tmp <- copy(tests)
tmp[,date:=as_date(collectedDate)]
tmp <- tmp[date >= make_date(2021, 1, 1)]
tmp[,patientId := factor(paste(as.numeric(patientId), infection, sep = "_"))]
tmp[,npos := sum(result), by = patientId]
cases <- tmp[!is.na(infection) & npos>=1, .SD[1], by = patientId]
cases <- cases[, .(n = .N), keyby = date]
cases[, avg := zoo::rollmean(n, 7, na.pad = TRUE)]
tmp <- tmp[!is.na(infection) & npos >= 2]


compute_agreement <- function(date, result, day, testType, dist_prev_neg){
  data.frame(date = date[-1],
             agreement = result[-1] == result[1], 
             lag = day[-1] - day[1], 
             first_test = rep(testType[1], length(testType)-1),
             test_type = testType[-1],
             dist_prev_neg = rep(dist_prev_neg[1], length(testType)-1))
}


res <- tmp[, compute_agreement(date, result, day, testType, dist_prev_neg),
           keyby = patientId]
res <- res[order(first_test, test_type, date, lag)]    

tmp2 <- res[, .(k = sum(agreement), n = .N), keyby = .(first_test, test_type, date, lag)]
tmp2 <- merge(CJ(date=seq(min(tmp2$date), max(tmp2$date), by = "day"),
                first_test = unique(tmp2$first_test),
                test_type = unique(tmp2$test_type),
                lag = 0:14),
              tmp2, by = c("first_test", "test_type", "date", "lag"), all.x = TRUE)

tmp2[is.na(tmp2)] <- 0
                
                
tmp2[,`:=`(k = zoo::rollsum(k, 30, na.pad = TRUE), n =  zoo::rollsum(n, 30, na.pad = TRUE)), 
           keyby = .(first_test, test_type, lag)]

tmp2[, prop := ifelse(n>0, k/n, 0)]
tmp2 <- merge(tmp2, cases[,c("date", "avg")], by = "date", all.x=TRUE)
tmp2 <- tmp2[!is.na(k) & !is.na(n) & !is.na(avg)]           

library(gridExtra)
library(animation)
saveGIF({
  for(i in unique(tmp2$date)){
    p1 <- cases |> 
      ggplot(aes(date, avg)) +
      geom_line() +
      geom_point(data = cases[date==i], color = "red") +
      ggtitle(paste0(as_date(i),":", format(round(cases[date==i]$avg/30), width=3),
                     " cases per 100,000")) + theme_bw()
    
     p2 <- tmp2[date==i] |> 
      ggplot(aes(lag, prop, color = test_type)) +
      geom_line() +
      geom_point() +
      ylim(c(0,1)) +
      facet_wrap(~first_test) +theme_bw()
    
  grid.arrange(p1, p2, nrow=2)

}}, movie.name = "test.gif", interval = .1)

