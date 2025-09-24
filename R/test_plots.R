
library(rjd3toolkit)
library(rjd3sax)
load("./Data/IP_US2")

s<-Retail$RetailSalesTotal

q1<-ltdarima_estimation(s, X=cbind(td(s=s, groups=c(1,1,1,1,1,1,0)), easter_variable(s=s)))
print(q1)

q1log<-ltdarima_estimation(log(s), X=cbind(td(s=s, groups=c(1,1,1,1,1,1,0)), easter_variable(s=s)))
print(q1log)

q2<-ltdarima_estimation(s, X=cbind(td(s=s, groups=c(1,1,1,1,1,1,0)), easter_variable(s=s)), fixed_var = FALSE)
print(q2)

q2log<-ltdarima_estimation(log(s), X=cbind(td(s=s, groups=c(1,1,1,1,1,1,0)), easter_variable(s=s)), fixed_var = FALSE)
print(q2log)

decomp<-q1log$final$decomposition$components
ts.plot(ts.union(decomp$trend_stdev, decomp$seas_stdev, decomp$irregular_stdev), col=c("blue", "red", "magenta"))

decomp<-q2log$final$decomposition$components
ts.plot(ts.union(decomp$trend_stdev, decomp$seas_stdev, decomp$irregular_stdev), col=c("blue", "red", "magenta"))
