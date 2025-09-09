# comparison of the two implementations of tdairline

tdairline_compare<-function(s){
  s<-rjd3toolkit::clean_extremities(s)
  q1<-rjd3sts::tdairline_estimation(s)

  l0<-q1$sarima$likelihood
  l1<-q1$ltd_sarima$likelihood

  q2<-rjd3sax::ltdarima_estimation(s, regular=c(0,1,1), seasonal=c(0,1,1), parametrization = "mean_delta")
  l2<-q2$final$likelihood$ll

  return(c(l0, l1, l2))
}

a<-sapply(rjd3toolkit::Retail, function(z){tdairline_compare(z)})

tdairline_compare2<-function(s){
  s<-rjd3toolkit::clean_extremities(s)
  q1<-rjd3sts::tdairline_estimation(s)

  l0<-q1$sarima$parameters
  l1<-q1$ltd_sarima$parameters

  q2<-rjd3sax::ltdarima_estimation(s, regular=c(0,1,1), seasonal=c(0,1,1))
  l2<-q2$final$model$parameters

  return(c(l0, l1, l2))
}

a<-sapply(rjd3toolkit::Retail, function(z){tdairline_compare(z)})
a<-t(a)
plot(a[,3]-a[,2], type='l')
