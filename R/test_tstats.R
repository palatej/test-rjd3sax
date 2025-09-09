s<-rjd3toolkit::Retail$RetailSalesTotal

q0<-rjd3sax::ltdarima_estimation(s, regular=c(0,1,1), seasonal=c(0,1,1), parametrization = "mean_delta")


p0<-q0$final$model$parameters
v0<-q0$final$model$covariance

print ("mean/delta")
print(p0)
print(p0/sqrt(diag(v0)))

p0mean<-q0$final$model$parima_mean
p0delta<-q0$final$model$parima_delta


q1<-rjd3sax::ltdarima_estimation(s, regular=c(0,1,1), seasonal=c(0,1,1), parametrization = "start_end")
p1<-q1$final$model$parameters
v1<-q1$final$model$covariance


T<-matrix(c(0.5,0,-1,0,0,0.5,0,-1,0.5,0,1,0,0,0.5,0,1), nrow = 4, ncol = 4)


p1c<-T%*%p1
v1c<-T%*%v1%*%t(T)

print ("mean/delta from start/end")
print(as.numeric(p1c))
print(as.numeric(p1c/sqrt(diag(v1c))))

print ("start/end")
print(p1)
print(p1/sqrt(diag(v1)))
