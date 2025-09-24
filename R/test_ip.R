ltd<-function(s, log=FALSE){
  if (is.null(s)) return (NULL)
  if (length(s)<36) return(NULL)
  if (any(is.na(s))) return(NULL)
  if (!any(s!=0)) return(NULL)
  if (log && any(s<=0)) return(NULL)
  if (log)s<-log(s)
  td<-rjd3toolkit::td(s=s)
  easter<-rjd3toolkit::easter_variable(s=s)

  q<-rjd3sax::ltdarima_estimation(s, regular=c(0,1,1), seasonal=c(0,1,1), X=cbind(td, easter), decomposition = FALSE)
  return(q)
}

ww<-NULL
zz<-NULL

daic<-function(s, title){
  z<-lapply(s, function(z){ltd(rjd3toolkit::clean_extremities(z))})
  sel<-sapply(z, is.null)
  if (any(sel))z<-z[-which(sel)]

  z<-sapply(z, function(r){r$initial$likelihood$aic-r$final$likelihood$aic})
  ww<<-c(ww,z)
  hist(z, breaks=15, main=title)

  print(sum(z>0)/length(z))

  z<-lapply(s, function(z){ltd(rjd3toolkit::clean_extremities(z), TRUE)})
  sel<-sapply(z, is.null)
  if (any(sel))z<-z[-which(sel)]

  z<-sapply(z, function(r){r$initial$likelihood$aic-r$final$likelihood$aic})
  zz<<-c(zz,z)
  hist(z, breaks=15, main=paste0(title, "(logs)"))

  print(sum(z>0)/length(z))

}

dbic<-function(s, title){
  z<-lapply(s, function(z){ltd(rjd3toolkit::clean_extremities(z))})
  sel<-sapply(z, is.null)
  if (any(sel))z<-z[-which(sel)]

  z<-sapply(z, function(r){r$initial$likelihood$bic-r$final$likelihood$bic})
  ww<<-c(ww,z)
  hist(z, breaks=15, main=title)

  print(sum(z>0)/length(z))

  z<-lapply(s, function(z){ltd(rjd3toolkit::clean_extremities(z), TRUE)})
  sel<-sapply(z, is.null)
  if (any(sel))z<-z[-which(sel)]

  z<-sapply(z, function(r){r$initial$likelihood$bic-r$final$likelihood$bic})
  zz<<-c(zz,z)
  hist(z, breaks=15, main=paste0(title, "(logs)"))

  print(sum(z>0)/length(z))

}

PVAL<-0.001

lrtest<-function(s, title){
  z<-lapply(s, function(z){ltd(rjd3toolkit::clean_extremities(z))})
  sel<-sapply(z, is.null)
  if (any(sel))z<-z[-which(sel)]

  z<-sapply(z, function(r){r$final$model$lr_test[2]})
  ww<<-c(ww,z)
  hist(z, breaks=15, main=title)

  print(sum(z<PVAL)/length(z))

  z<-lapply(s, function(z){ltd(rjd3toolkit::clean_extremities(z), TRUE)})
  sel<-sapply(z, is.null)
  if (any(sel))z<-z[-which(sel)]

  z<-sapply(z, function(r){r$final$model$lr_test[2]})
  zz<<-c(zz,z)
  hist(z, breaks=15, main=paste0(title, "(logs)"))

  print(sum(z<PVAL)/length(z))

}


load("./Data/IP_US")
lrtest(IP_Us, "US")

load("./Data/IP_US2")
lrtest(IP_Us2, "US2")

load("./Data/IP_BELGIUM")
lrtest(IP_Belgium, "Belgium")

load("./Data/IP_GERMANY")
lrtest(IP_Germany, "Germany")

load("./Data/IP_FRANCE")
lrtest(IP_France, "France")

load("./Data/IP_SPAIN")
lrtest(IP_Spain, "Spain")

load("./Data/IP_ITALY")
lrtest(IP_Italy, "Italy")

print(sum(zz<PVAL)/length(zz))
print(sum(ww<PVAL)/length(ww))
hist(zz, breaks=50, main="all")
hist(ww, breaks=50, main="all (logs)")
