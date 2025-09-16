

ltd1<-function(s){
  if (is.null(s)) return (NULL)
  td<-rjd3toolkit::td(s=s)
  easter<-rjd3toolkit::easter_variable(s=s)

  q<-rjd3sax::ltdarima_estimation(s, regular=c(0,1,1), seasonal=c(0,1,1), X=cbind(td, easter), eps=1e-15, decomposition=FALSE)
  return(q)
}

z<-sapply(rjd3toolkit::Retail, function(z){r<-ltd1(rjd3toolkit::clean_extremities(z)); return(r$initial$likelihood$aic-r$final$likelihood$aic)})
hist(z, n=10)

z<-sapply(rjd3toolkit::Retail, function(z){r<-ltd1(log(rjd3toolkit::clean_extremities(z))); return(r$initial$likelihood$aic-r$final$likelihood$aic)})
hist(z, n=10)
