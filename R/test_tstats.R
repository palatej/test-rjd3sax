s<-rjd3toolkit::Retail$RetailSalesTotal

q0<-rjd3sax::ltdarima_estimation(log(s), regular=c(0,1,1), seasonal=c(0,1,1), parametrization = "mean_delta")


p0<-q0$final$model$parameters
v0<-q0$final$model$covariance

print ("mean/delta")
print(p0)
print(p0/sqrt(diag(v0)))

q1<-rjd3sax::ltdarima_estimation(log(s), regular=c(0,1,1), seasonal=c(0,1,1), parametrization = "start_end")
p1<-q1$final$model$derived_parameters
v1<-q1$final$model$derived_parameters_covariance

print ("mean/delta from start/end")
print(p1)
print(p1/sqrt(diag(v1)))
