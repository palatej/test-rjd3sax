s<-rjd3toolkit::Retail$RetailSalesTotal
#s<-log(s)

td<-rjd3toolkit::td(s=s)

q1<-rjd3sax::ltdarima_estimation(s, X=td)
q2<-rjd3sax::ltdarima_estimation(s, X=td, fixed_var = FALSE)
q3<-rjd3sax::ltdarima_estimation(s, X=td, fixed_var = FALSE, parametrization = "start_end")

print(q1)
print(q2)

ts.plot(window(ts.union(q1$final$decomposition$components$sa, q2$final$decomposition$components$sa), start=2005), col=c("red", "blue"))
plot((q1$final$decomposition$components$seas-q2$final$decomposition$components$seas), col="red")
ts.plot(ts.union(q1$final$decomposition$components$seas_stdev, q2$final$decomposition$components$seas_stdev), col=c("red", "blue"))

ts.plot(window(ts.union(q1$initial$decomposition$components$sa, q1$final$decomposition$components$sa), start=2005), col=c("red", "blue"))
ts.plot(window(ts.union(q2$initial$decomposition$components$sa, q2$final$decomposition$components$sa), start=2005), col=c("red", "blue"))
